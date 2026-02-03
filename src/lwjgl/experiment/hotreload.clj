(ns lwjgl.experiment.hotreload
  (:gen-class)
  (:require [clojure.core.async :as async]
            [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWMouseButtonCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)
           (org.joml Matrix4f)))

;; ---- 运行时状态 ---------------------------------------------------------

(defonce cmd-chan (async/chan 128))
(defonce render-fn (atom nil))
(defonce key-handler (atom nil))
(defonce command-table (atom {}))
(defonce clear-color (atom [0.2 0.3 0.3 1.0]))
(defonce axis-style
  (atom {:length 1.0
         :arrow-length 0.12
         :arrow-radius 0.05
         :line-width 2.0}))
(defonce app
  (atom {:window 0
         :time {:now 0.0 :dt 0.0 :frame 0 :last 0.0}
         :scene {:key :baseline :state nil :since 0.0}
         :timeline {:enabled? false :items [] :index 0 :elapsed 0.0}
         :transition {:active? false :alpha 0.0 :duration 0.6}
         :camera {:yaw 0.0 :pitch 0.0}
         :input {:fb-width 800 :fb-height 600
                 :win-width 800 :win-height 600
                 :dragging? false :last-x 0.0 :last-y 0.0}
         :flags {:paused? false}}))

;; 立方体状态 - 每个立方体包含 :pos [x y z]、:rot [rx ry rz]、:scale [sx sy sz]、:color [r g b]
(defonce cubes (atom []))
(defonce next-cube-id (atom 0))

(defonce scenes (atom {}))
(defonce point-cloud-state (atom {:points [] :colors []}))
(defonce rig-state (atom {:segments []}))
(defonce mesh-style
  (atom {:pos [1.6 0.0 0.0]
         :rot [0.0 0.0 0.0]
         :scale [0.8 0.8 0.8]
         :color [0.2 0.6 0.9]}))

(defonce state
  (atom {:program 0
         :vao 0
         :vbo 0
         :ebo 0
         :tex 0
         :index-count 0
         :axis-program 0
         :axis-line-vao 0
         :axis-line-vbo 0
         :axis-line-count 0
         :axis-arrow-vao 0
         :axis-arrow-vbo 0
         :axis-arrow-count 0
         ;; 立方体渲染资源
         :cube-program 0
         :cube-vao 0
         :cube-vbo 0
         :cube-vertex-count 0
         ;; 点云渲染资源
         :point-program 0
         :point-vao 0
         :point-vbo 0
         :point-count 0
         ;; 通用网格资源
         :mesh-vao 0
         :mesh-vbo 0
         :mesh-ebo 0
         :mesh-index-count 0
         ;; 弹簧线段
         :spring-vao 0
         :spring-vbo 0
         :spring-count 0
         ;; 过场覆盖层
         :overlay-program 0
         :overlay-vao 0
         :overlay-vbo 0}))

(defonce vs-source
  (atom
   "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
uniform mat4 mvp;
out vec2 TexCoord;
void main() {
    gl_Position = mvp * vec4(aPos, 1.0);
    TexCoord = aTexCoord;
}"))

(defonce fs-source
  (atom
   "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform sampler2D texture1;
void main() {
    FragColor = texture(texture1, TexCoord);
}"))

(def ^:private axis-vs-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
uniform mat4 mvp;
out vec3 vColor;
void main() {
    vColor = aColor;
    gl_Position = mvp * vec4(aPos, 1.0);
}")

(def ^:private axis-fs-source
  "#version 330 core
in vec3 vColor;
out vec4 FragColor;
void main() {
    FragColor = vec4(vColor, 1.0);
}")

(def ^:private point-vs-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
uniform mat4 mvp;
out vec3 vColor;
void main() {
    vColor = aColor;
    gl_Position = mvp * vec4(aPos, 1.0);
}")

(def ^:private point-fs-source
  "#version 330 core
in vec3 vColor;
out vec4 FragColor;
void main() {
    FragColor = vec4(vColor, 1.0);
}")

(def ^:private overlay-vs-source
  "#version 330 core
layout (location = 0) in vec2 aPos;
void main() {
    gl_Position = vec4(aPos, 0.0, 1.0);
}")

(def ^:private overlay-fs-source
  "#version 330 core
uniform vec4 uColor;
out vec4 FragColor;
void main() {
    FragColor = uColor;
}")

;; ---- 立方体几何数据 ---------------------------------------------------------

(def ^:private cube-vertices
  (float-array
   [;; position xyz, normal xyz
    ;; front
    -0.5 -0.5  0.5  0.0  0.0  1.0
    0.5 -0.5  0.5  0.0  0.0  1.0
    0.5  0.5  0.5  0.0  0.0  1.0
    0.5  0.5  0.5  0.0  0.0  1.0
    -0.5  0.5  0.5  0.0  0.0  1.0
    -0.5 -0.5  0.5  0.0  0.0  1.0
    ;; back
    -0.5 -0.5 -0.5  0.0  0.0 -1.0
    -0.5  0.5 -0.5  0.0  0.0 -1.0
    0.5  0.5 -0.5  0.0  0.0 -1.0
    0.5  0.5 -0.5  0.0  0.0 -1.0
    0.5 -0.5 -0.5  0.0  0.0 -1.0
    -0.5 -0.5 -0.5  0.0  0.0 -1.0
    ;; left
    -0.5  0.5  0.5 -1.0  0.0  0.0
    -0.5  0.5 -0.5 -1.0  0.0  0.0
    -0.5 -0.5 -0.5 -1.0  0.0  0.0
    -0.5 -0.5 -0.5 -1.0  0.0  0.0
    -0.5 -0.5  0.5 -1.0  0.0  0.0
    -0.5  0.5  0.5 -1.0  0.0  0.0
    ;; right
    0.5  0.5  0.5  1.0  0.0  0.0
    0.5 -0.5 -0.5  1.0  0.0  0.0
    0.5  0.5 -0.5  1.0  0.0  0.0
    0.5 -0.5 -0.5  1.0  0.0  0.0
    0.5  0.5  0.5  1.0  0.0  0.0
    0.5 -0.5  0.5  1.0  0.0  0.0
    ;; top
    -0.5  0.5 -0.5  0.0  1.0  0.0
    -0.5  0.5  0.5  0.0  1.0  0.0
    0.5  0.5  0.5  0.0  1.0  0.0
    0.5  0.5  0.5  0.0  1.0  0.0
    0.5  0.5 -0.5  0.0  1.0  0.0
    -0.5  0.5 -0.5  0.0  1.0  0.0
    ;; bottom
    -0.5 -0.5 -0.5  0.0 -1.0  0.0
    0.5 -0.5  0.5  0.0 -1.0  0.0
    -0.5 -0.5  0.5  0.0 -1.0  0.0
    0.5 -0.5  0.5  0.0 -1.0  0.0
    -0.5 -0.5 -0.5  0.0 -1.0  0.0
    0.5 -0.5 -0.5  0.0 -1.0  0.0]))

(def ^:private cube-vs-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
out vec3 vNormal;
out vec3 vPos;
void main() {
    vNormal = mat3(transpose(inverse(model))) * aNormal;
    vPos = vec3(model * vec4(aPos, 1.0));
    gl_Position = projection * view * model * vec4(aPos, 1.0);
}")

(def ^:private cube-fs-source
  "#version 330 core
in vec3 vNormal;
in vec3 vPos;
uniform vec3 uColor;
uniform vec3 uLightPos;
uniform vec3 uViewPos;
out vec4 FragColor;
void main() {
    vec3 norm = normalize(vNormal);
    vec3 lightDir = normalize(uLightPos - vPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 viewDir = normalize(uViewPos - vPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32.0);
    vec3 ambient = 0.3 * uColor;
    vec3 diffuse = diff * uColor;
    vec3 specular = vec3(0.3) * spec;
    FragColor = vec4(ambient + diffuse + specular, 1.0);
}")

(declare ensure-default-scenes!
         upload-point-cloud!
         upload-spring-lines!
         set-point-cloud!
         clear-point-cloud!
         set-mesh!
         set-mesh-style!
         clear-spring-lines!
         clear-mesh!)

(defn enqueue!
  "将一个函数加入队列在 OpenGL/主线程上运行。返回一个 promise-chan，
  它会返回 {:ok value} 或 {:err throwable}。"
  [f]
  (let [reply (async/promise-chan)]
    (async/put! cmd-chan
                (fn []
                  (try
                    (async/put! reply {:ok (f)})
                    (catch Throwable t
                      (async/put! reply {:err t})))))
    (GLFW/glfwPostEmptyEvent)
    reply))

(defn- drain-commands!
  []
  (loop []
    (when-let [f (async/poll! cmd-chan)]
      (f)
      (recur))))

;; ---- 场景管理与时间线 -----------------------------------------------------

(defn- now-seconds
  []
  (GLFW/glfwGetTime))

(defn- scene-context
  []
  {:window (:window @app)
   :resources @state
   :time (:time @app)
   :scene (:scene @app)
   :timeline (:timeline @app)
   :camera (:camera @app)
   :input (:input @app)
   :flags (:flags @app)
   :params {:clear-color @clear-color
            :axis-style @axis-style}
   :cubes @cubes})

(defn- current-scene-def
  []
  (get @scenes (get-in @app [:scene :key])))

(defn- apply-scene!
  [scene-key]
  (let [scene (get @scenes scene-key)]
    (when-not scene
      (throw (ex-info "未找到场景" {:scene scene-key})))
    (let [{:keys [key state]} (:scene @app)
          old-scene (get @scenes key)
          ctx (scene-context)]
      (when-let [cleanup (:cleanup old-scene)]
        (cleanup ctx state))
      (swap! app assoc :scene {:key scene-key :state nil :since 0.0})
      (let [init (:init scene)
            init-ctx (scene-context)
            new-state (when init (init init-ctx))]
        (swap! app assoc-in [:scene :state] new-state))
      scene-key)))

(defn register-scene!
  "注册一个场景，scene 需要包含 :init/:update/:render/:cleanup 中的任意组合。"
  [scene-key scene]
  (swap! scenes assoc scene-key scene)
  scene-key)

(defn list-scenes
  "返回已注册的场景 key 列表。"
  []
  (vec (keys @scenes)))

(defn current-scene
  "返回当前场景 key。"
  []
  (get-in @app [:scene :key]))

(defn set-scene!
  "切换到指定场景。该操作在 GL 线程中执行。"
  [scene-key]
  (ensure-default-scenes!)
  (enqueue! #(apply-scene! scene-key)))

(defn set-timeline!
  "设置时间线。items 形如 {:scene :foo :duration 5.0} 的向量。"
  [items]
  (swap! app assoc :timeline {:enabled? false
                              :items (vec items)
                              :index 0
                              :elapsed 0.0})
  :ok)

(defn start-timeline!
  "启动时间线并切换到首个场景。"
  []
  (let [items (get-in @app [:timeline :items])]
    (when (seq items)
      (swap! app assoc-in [:timeline :enabled?] true)
      (set-scene! (:scene (first items))))))

(defn stop-timeline!
  "停止时间线推进。"
  []
  (swap! app assoc-in [:timeline :enabled?] false))

(defn set-transition!
  "设置过场时长（秒）。"
  [duration]
  (swap! app assoc-in [:transition :duration] (max 0.0 (double duration)))
  :ok)

(defn trigger-transition!
  "手动触发一次过场遮罩。"
  []
  (swap! app assoc :transition {:active? true
                                :alpha 1.0
                                :duration (get-in @app [:transition :duration])})
  :ok)

(defn pause!
  "暂停场景更新。"
  []
  (swap! app assoc-in [:flags :paused?] true))

(defn resume!
  "恢复场景更新。"
  []
  (swap! app assoc-in [:flags :paused?] false))

(defn register-command!
  "注册键盘命令。action 可以是 GLFW/GLFW_PRESS 等。"
  [key action f]
  (swap! command-table assoc [key action] f))

(defn- update-time!
  []
  (let [now (now-seconds)
        last (get-in @app [:time :last])
        dt (max 0.0 (- now last))
        paused? (get-in @app [:flags :paused?])
        dt (if paused? 0.0 dt)]
    (swap! app assoc :time {:now now
                            :dt dt
                            :frame (inc (get-in @app [:time :frame]))
                            :last now})))

(defn- update-scene!
  [dt]
  (swap! app update-in [:scene :since] + dt)
  (when-not (get-in @app [:flags :paused?])
    (when-let [update-fn (:update (current-scene-def))]
      (let [ctx (scene-context)
            scene-state (get-in @app [:scene :state])
            new-state (update-fn ctx scene-state)]
        (swap! app assoc-in [:scene :state] new-state)))))

(defn- update-timeline!
  [dt]
  (let [{:keys [enabled? items index elapsed]} (:timeline @app)]
    (when (and enabled? (seq items))
      (let [elapsed (+ elapsed dt)
            {:keys [duration]} (nth items index)]
        (if (>= elapsed duration)
          (let [next-index (mod (inc index) (count items))
                next-scene (:scene (nth items next-index))]
            (swap! app assoc-in [:timeline :index] next-index)
            (swap! app assoc-in [:timeline :elapsed] 0.0)
            (apply-scene! next-scene)
            (swap! app assoc :transition {:active? true
                                          :alpha 1.0
                                          :duration (get-in @app [:transition :duration])}))
          (swap! app assoc-in [:timeline :elapsed] elapsed))))))

(defn- update-transition!
  [dt]
  (let [{:keys [active? alpha duration]} (:transition @app)]
    (when active?
      (let [step (if (pos? duration) (/ dt duration) 1.0)
            alpha (max 0.0 (- alpha step))]
        (if (<= alpha 0.0)
          (swap! app assoc :transition {:active? false :alpha 0.0 :duration duration})
          (swap! app assoc-in [:transition :alpha] alpha))))))

(defn- render-scene!
  []
  (let [ctx (scene-context)
        scene-state (get-in @app [:scene :state])]
    (if-let [override @render-fn]
      (override (:resources ctx))
      (when-let [render-fn (:render (current-scene-def))]
        (render-fn ctx scene-state)))))

(defn- render-transition!
  []
  (let [{:keys [active? alpha]} (:transition @app)]
    (when (and active? (pos? alpha))
      (let [{:keys [overlay-program overlay-vao]} @state
            color-loc (GL20/glGetUniformLocation overlay-program "uColor")]
        (GL11/glDisable GL11/GL_DEPTH_TEST)
        (GL11/glEnable GL11/GL_BLEND)
        (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
        (GL20/glUseProgram overlay-program)
        (when (<= 0 color-loc)
          (GL20/glUniform4f color-loc 0.0 0.0 0.0 (float alpha)))
        (GL30/glBindVertexArray overlay-vao)
        (GL11/glDrawArrays GL11/GL_TRIANGLES 0 6)
        (GL30/glBindVertexArray 0)
        (GL11/glDisable GL11/GL_BLEND)
        (GL11/glEnable GL11/GL_DEPTH_TEST)))))

;; ---- OpenGL 辅助函数 -----------------------------------------------------------

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- clamp01
  [v]
  (min 1.0 (max 0.0 v)))

(defn vec3
  "创建三维向量。"
  [x y z]
  [(float x) (float y) (float z)])

(defn color3
  "创建颜色，自动限制在 0-1 范围内。"
  [r g b]
  [(float (clamp01 r)) (float (clamp01 g)) (float (clamp01 b))])

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.rewind buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn- create-quad
  []
  (let [vertices (float-array
                  [;; positions       ;; tex coords
                   0.5  0.5  0.0      1.0 1.0
                   0.5 -0.5  0.0      1.0 0.0
                   -0.5 -0.5  0.0      0.0 0.0
                   -0.5  0.5  0.0      0.0 1.0])
        indices (int-array [0 1 3 1 2 3])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        ebo (GL15/glGenBuffers)
        vbuf (BufferUtils/createFloatBuffer (alength vertices))
        ibuf (BufferUtils/createIntBuffer (alength indices))
        stride (* 5 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put vbuf vertices)
    (.flip vbuf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER vbuf GL15/GL_STATIC_DRAW)
    (.put ibuf indices)
    (.flip ibuf)
    (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER ibuf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :ebo ebo :index-count 6}))

(defn- create-texture
  [w h]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [checker (if (zero? (bit-and (+ (quot x 16) (quot y 16)) 1)) 255 80)
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (unchecked-byte checker))
          (.put buf (inc idx) (unchecked-byte checker))
          (.put buf (+ idx 2) (unchecked-byte 220)))))
    (.rewind buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))

(defn- build-axis-vao
  [vertices]
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        vbuf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put vbuf vertices)
    (.flip vbuf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER vbuf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count (int (/ (alength vertices) 6))}))

(defn- build-point-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER (* 6 Float/BYTES) GL15/GL_DYNAMIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn- build-overlay-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        vertices (float-array
                  [;; full screen quad in NDC
                   -1.0 -1.0
                   1.0 -1.0
                   1.0  1.0
                   1.0  1.0
                   -1.0  1.0
                   -1.0 -1.0])
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 2 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn- build-mesh-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        ebo (GL15/glGenBuffers)
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER 0 GL15/GL_DYNAMIC_DRAW)
    (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER 0 GL15/GL_DYNAMIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :ebo ebo}))

(defn- build-spring-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER 0 GL15/GL_DYNAMIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn- create-cube-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        vbuf (BufferUtils/createFloatBuffer (alength cube-vertices))
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put vbuf cube-vertices)
    (.flip vbuf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER vbuf GL15/GL_STATIC_DRAW)
    ;; 顶点位置
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    ;; 法线
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count (/ (alength cube-vertices) 6)}))

(defn uv-sphere
  "生成 UV 球体网格，返回 {:vertices float-array :indices int-array}。
  参数：
    lat-segs - 纬向分段数
    lon-segs - 经向分段数"
  [lat-segs lon-segs]
  (let [lat-segs (max 3 (int lat-segs))
        lon-segs (max 3 (int lon-segs))
        vertex-count (* (inc lat-segs) (inc lon-segs))
        vertices (transient [])
        indices (transient [])]
    (dotimes [i (inc lat-segs)]
      (let [v (/ (double i) lat-segs)
            phi (* Math/PI v)
            y (Math/cos phi)
            r (Math/sin phi)]
        (dotimes [j (inc lon-segs)]
          (let [u (/ (double j) lon-segs)
                theta (* 2.0 Math/PI u)
                x (* r (Math/cos theta))
                z (* r (Math/sin theta))
                nx x
                ny y
                nz z]
            (conj! vertices (float x))
            (conj! vertices (float y))
            (conj! vertices (float z))
            (conj! vertices (float nx))
            (conj! vertices (float ny))
            (conj! vertices (float nz))))))
    (dotimes [i lat-segs]
      (dotimes [j lon-segs]
        (let [row-a (* i (inc lon-segs))
              row-b (* (inc i) (inc lon-segs))
              a (+ row-a j)
              b (+ row-b j)
              c (+ row-b (inc j))
              d (+ row-a (inc j))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))
     :vertex-count vertex-count}))

(defn make-point-cloud
  "创建点云数据。传入 points 与可选 colors。
  points 为 [[x y z] ...]。
  colors 为 [[r g b] ...]，若不足则使用白色。"
  [points & {:keys [colors]}]
  {:points (vec points)
   :colors (vec (or colors []))})

(defn sphere-point-cloud
  "生成球面点云。"
  [count radius]
  (let [count (max 1 (int count))
        radius (double radius)]
    (vec
     (repeatedly count
                 (fn []
                   (let [u (rand)
                         v (rand)
                         theta (* 2.0 Math/PI u)
                         phi (Math/acos (- 1.0 (* 2.0 v)))
                         x (* radius (Math/sin phi) (Math/cos theta))
                         y (* radius (Math/cos phi))
                         z (* radius (Math/sin phi) (Math/sin theta))]
                     [(float x) (float y) (float z)]))))))

(defn- compose-transform
  [pos rot scale]
  (doto (Matrix4f.)
    (.identity)
    (.translate (float (nth pos 0)) (float (nth pos 1)) (float (nth pos 2)))
    (.rotateX (float (Math/toRadians (nth rot 0))))
    (.rotateY (float (Math/toRadians (nth rot 1))))
    (.rotateZ (float (Math/toRadians (nth rot 2))))
    (.scale (float (nth scale 0)) (float (nth scale 1)) (float (nth scale 2)))))

(defn- chain-transform
  [parent pos rot scale]
  (doto (Matrix4f. parent)
    (.translate (float (nth pos 0)) (float (nth pos 1)) (float (nth pos 2)))
    (.rotateX (float (Math/toRadians (nth rot 0))))
    (.rotateY (float (Math/toRadians (nth rot 1))))
    (.rotateZ (float (Math/toRadians (nth rot 2))))
    (.scale (float (nth scale 0)) (float (nth scale 1)) (float (nth scale 2)))))

(defn- rand-range
  [a b]
  (+ a (* (rand) (- b a))))

(defn- respawn-particle
  [radius]
  (let [angle (* 2.0 Math/PI (rand))
        r (rand-range (* 0.2 radius) radius)
        x (* r (Math/cos angle))
        z (* r (Math/sin angle))
        y (rand-range -0.2 0.2)
        vx (rand-range -0.3 0.3)
        vy (rand-range 0.1 0.6)
        vz (rand-range -0.3 0.3)]
    {:pos [(float x) (float y) (float z)]
     :vel [(float vx) (float vy) (float vz)]
     :life (rand-range 1.5 4.0)
     :color [(float (rand)) (float (rand)) (float (rand))]}))

(defn- init-particles
  [count radius]
  (vec (repeatedly (max 1 (int count)) #(respawn-particle radius))))

(defn- update-particles
  [particles dt t]
  (let [target [(float (* 0.5 (Math/sin (* 0.6 t))))
                (float (* 0.3 (Math/cos (* 0.4 t))))
                0.0]
        gravity [0.0 -0.25 0.0]]
    (mapv
     (fn [{:keys [pos vel life] :as p}]
       (let [[px py pz] pos
             [vx vy vz] vel
             [tx ty tz] target
             dx (- tx px)
             dy (- ty py)
             dz (- tz pz)
             dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 0.001))
             att-scale (/ 0.6 (+ 0.2 (* dist dist)))
             ax (+ (nth gravity 0) (* att-scale (/ dx dist)) (* 0.15 (- pz)))
             ay (+ (nth gravity 1) (* att-scale (/ dy dist)))
             az (+ (nth gravity 2) (* att-scale (/ dz dist)) (* 0.15 px))
             nvx (+ vx (* ax dt))
             nvy (+ vy (* ay dt))
             nvz (+ vz (* az dt))
             npx (+ px (* nvx dt))
             npy (+ py (* nvy dt))
             npz (+ pz (* nvz dt))
             life (- life dt)]
         (if (or (<= life 0.0) (> (Math/abs npx) 4.0) (> (Math/abs npy) 4.0) (> (Math/abs npz) 4.0))
           (respawn-particle 1.6)
           (assoc p :pos [(float npx) (float npy) (float npz)]
                  :vel [(float nvx) (float nvy) (float nvz)]
                  :life life))))
     particles)))

(defn- particles->points
  [particles]
  (let [points (mapv :pos particles)
        colors (mapv :color particles)]
    {:points points :colors colors}))

(defn- spring-color
  [ratio]
  (let [ratio (max -1.0 (min 1.0 ratio))]
    (if (pos? ratio)
      [(+ 0.2 (* 0.8 ratio))
       (+ 0.8 (* -0.6 ratio))
       0.2]
      [0.2
       (+ 0.8 (* -0.6 (- ratio)))
       (+ 0.2 (* 0.8 (- ratio)))])))

(defn- make-spring-grid
  [rows cols spacing]
  (let [rows (max 2 (int rows))
        cols (max 2 (int cols))
        spacing (double spacing)
        half-w (* 0.5 spacing (dec cols))
        half-d (* 0.5 spacing (dec rows))
        idx (fn [r c] (+ (* r cols) c))
        nodes (vec
               (for [r (range rows)
                     c (range cols)]
                 (let [x (- (* c spacing) half-w)
                       z (- (* r spacing) half-d)
                       y 0.6
                       fixed? (= r 0)]
                   {:pos [(float x) (float y) (float z)]
                    :vel [0.0 0.0 0.0]
                    :fixed? fixed?})))
        springs (transient [])]
    (doseq [r (range rows)
            c (range cols)]
      (let [i (idx r c)]
        (when (< c (dec cols))
          (let [j (idx r (inc c))]
            (conj! springs [i j spacing])))
        (when (< r (dec rows))
          (let [j (idx (inc r) c)]
            (conj! springs [i j spacing])))
        (when (and (< r (dec rows)) (< c (dec cols)))
          (let [j (idx (inc r) (inc c))]
            (conj! springs [i j (* spacing (Math/sqrt 2.0))])))
        (when (and (< r (dec rows)) (> c 0))
          (let [j (idx (inc r) (dec c))]
            (conj! springs [i j (* spacing (Math/sqrt 2.0))])))))
    {:rows rows :cols cols :nodes nodes :springs (persistent! springs)}))

(defn- update-spring-grid
  [nodes springs dt]
  (let [k 8.0
        damping 0.4
        gravity [0.0 -1.4 0.0]
        forces (vec (repeat (count nodes) [0.0 0.0 0.0]))]
    (let [forces
          (reduce
           (fn [fs [i j rest]]
             (let [{:keys [pos]} (nth nodes i)
                   {pos2 :pos} (nth nodes j)
                   [x1 y1 z1] pos
                   [x2 y2 z2] pos2
                   dx (- x2 x1)
                   dy (- y2 y1)
                   dz (- z2 z1)
                   dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-6))
                   diff (- dist rest)
                   f (* k diff)
                   fx (* f (/ dx dist))
                   fy (* f (/ dy dist))
                   fz (* f (/ dz dist))
                   [f1x f1y f1z] (nth fs i)
                   [f2x f2y f2z] (nth fs j)]
               (assoc fs
                      i [(+ f1x fx) (+ f1y fy) (+ f1z fz)]
                      j [(+ f2x (- fx)) (+ f2y (- fy)) (+ f2z (- fz))])))
           forces
           springs)]
      (mapv
       (fn [{:keys [pos vel fixed?] :as n} [fx fy fz]]
         (if fixed?
           (assoc n :vel [0.0 0.0 0.0])
           (let [[vx vy vz] vel
                 ax (+ fx (nth gravity 0) (* (- damping) vx))
                 ay (+ fy (nth gravity 1) (* (- damping) vy))
                 az (+ fz (nth gravity 2) (* (- damping) vz))
                 nvx (+ vx (* ax dt))
                 nvy (+ vy (* ay dt))
                 nvz (+ vz (* az dt))
                 [px py pz] pos
                 npx (+ px (* nvx dt))
                 npy (+ py (* nvy dt))
                 npz (+ pz (* nvz dt))]
             (assoc n :pos [(float npx) (float npy) (float npz)]
                    :vel [(float nvx) (float nvy) (float nvz)]))))
       nodes
       forces))))

(defn- spring-lines
  [nodes springs]
  (let [data (transient [])]
    (doseq [[i j rest] springs]
      (let [{:keys [pos]} (nth nodes i)
            {pos2 :pos} (nth nodes j)
            [x1 y1 z1] pos
            [x2 y2 z2] pos2
            dx (- x2 x1)
            dy (- y2 y1)
            dz (- z2 z1)
            dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-6))
            ratio (/ (- dist rest) (max rest 1.0e-6))
            [r g b] (spring-color ratio)]
        (conj! data (float x1))
        (conj! data (float y1))
        (conj! data (float z1))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))
        (conj! data (float x2))
        (conj! data (float y2))
        (conj! data (float z2))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))))
    (persistent! data)))
(defn- sdf-sphere
  [x y z [cx cy cz] r]
  (let [dx (- x cx)
        dy (- y cy)
        dz (- z cz)]
    (- (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) r)))

(defn- smooth-min
  "平滑最小值（k 越大越平滑）。"
  [^double a ^double b ^double k]
  (let [h (clamp01 (+ 0.5 (/ (- b a) (* 2.0 k))))]
    (- (min a b) (* k h (- 1.0 h)))))

(defn- sdf-metaballs
  [^double x ^double y ^double z t]
  (let [c1 [(+ -0.6 (* 0.6 (Math/sin (* 0.7 t))))
            (* 0.4 (Math/cos (* 0.5 t)))
            0.0]
        c2 [(+ 0.6 (* 0.4 (Math/sin (* 0.9 t))))
            (* -0.3 (Math/cos (* 0.6 t)))
            0.2]
        c3 [0.0 (* 0.5 (Math/sin (* 0.4 t))) -0.4]
        d1 (sdf-sphere x y z c1 0.6)
        d2 (sdf-sphere x y z c2 0.55)
        d3 (sdf-sphere x y z c3 0.5)]
    (-> d1
        (smooth-min d2 0.4)
        (smooth-min d3 0.4))))

(defn- sdf-gradient
  [f x y z t]
  (let [e 0.002
        dx (- (f (+ x e) y z t) (f (- x e) y z t))
        dy (- (f x (+ y e) z t) (f x (- y e) z t))
        dz (- (f x y (+ z e) t) (f x y (- z e) t))
        len (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-9))]
    [(/ dx len) (/ dy len) (/ dz len)]))

(def ^:private tetra-indices
  [[0 5 1 6]
   [0 1 2 6]
   [0 2 3 6]
   [0 3 7 6]
   [0 7 4 6]
   [0 4 5 6]])

(def ^:private cube-offsets
  [[0 0 0]
   [1 0 0]
   [1 1 0]
   [0 1 0]
   [0 0 1]
   [1 0 1]
   [1 1 1]
   [0 1 1]])

(defn- interp-point
  [p1 p2 v1 v2]
  (let [t (/ v1 (- v1 v2))
        [x1 y1 z1] p1
        [x2 y2 z2] p2]
    [(+ x1 (* t (- x2 x1)))
     (+ y1 (* t (- y2 y1)))
     (+ z1 (* t (- z2 z1)))]))

(defn- marching-tetra
  [points values f t vertices indices]
  (let [edges [[0 1] [1 2] [2 0] [0 3] [1 3] [2 3]]
        crosses (reduce
                 (fn [acc [a b]]
                   (let [va (nth values a)
                         vb (nth values b)]
                     (if (<= (* va vb) 0.0)
                       (conj acc (interp-point (nth points a) (nth points b) va vb))
                       acc)))
                 []
                 edges)]
    (cond
      (= 3 (count crosses))
      (let [base (quot (count @vertices) 6)]
        (doseq [p crosses]
          (let [[nx ny nz] (sdf-gradient f (nth p 0) (nth p 1) (nth p 2) t)]
            (swap! vertices into [(float (nth p 0)) (float (nth p 1)) (float (nth p 2))
                                  (float nx) (float ny) (float nz)])))
        (swap! indices into [base (inc base) (+ base 2)]))
      (= 4 (count crosses))
      (let [base (quot (count @vertices) 6)
            order [[0 1 2] [0 2 3]]]
        (doseq [p crosses]
          (let [[nx ny nz] (sdf-gradient f (nth p 0) (nth p 1) (nth p 2) t)]
            (swap! vertices into [(float (nth p 0)) (float (nth p 1)) (float (nth p 2))
                                  (float nx) (float ny) (float nz)])))
        (swap! indices into
               (mapcat (fn [[a b c]] [(+ base a) (+ base b) (+ base c)]) order)))
      :else nil)))

(defn marching-tetrahedra
  "使用四面体分割的 marching 算法生成网格。"
  [f {:keys [min max res t]}]
  (let [[minx miny minz] min
        [maxx maxy maxz] max
        res (clojure.core/max 4 (int res))
        step-x (/ (- maxx minx) res)
        step-y (/ (- maxy miny) res)
        step-z (/ (- maxz minz) res)
        vertices (atom [])
        indices (atom [])]
    (dotimes [i res]
      (dotimes [j res]
        (dotimes [k res]
          (let [base [i j k]
                cube-points (mapv (fn [[ox oy oz]]
                                    [(+ minx (* (+ i ox) step-x))
                                     (+ miny (* (+ j oy) step-y))
                                     (+ minz (* (+ k oz) step-z))])
                                  cube-offsets)
                cube-values (mapv (fn [[x y z]] (f x y z t)) cube-points)]
            (doseq [[a b c d] tetra-indices]
              (marching-tetra [(nth cube-points a)
                               (nth cube-points b)
                               (nth cube-points c)
                               (nth cube-points d)]
                              [(nth cube-values a)
                               (nth cube-values b)
                               (nth cube-values c)
                               (nth cube-values d)]
                              f t vertices indices))))))
    {:vertices (float-array @vertices)
     :indices (int-array @indices)}))

(defn sweep-mesh
  "生成沿 X 轴扫掠的管状网格。返回 {:vertices float-array :indices int-array}。
  参数：
    :segments  路径分段
    :ring      环形分段
    :length    路径长度
    :radius    基础半径
    :amp       路径摆动幅度
    :freq      摆动频率
    :twist     扭转角度（度）"
  [& {:keys [segments ring length radius amp freq twist]
      :or {segments 80 ring 16 length 3.0 radius 0.25 amp 0.35 freq 1.5 twist 0.0}}]
  (let [segments (max 3 (int segments))
        ring (max 6 (int ring))
        length (double length)
        radius (double radius)
        amp (double amp)
        freq (double freq)
        twist (Math/toRadians (double twist))
        vertices (transient [])
        indices (transient [])]
    (dotimes [i (inc segments)]
      (let [t (/ (double i) segments)
            x (- (* t length) (* 0.5 length))
            phase (* 2.0 Math/PI freq t)
            y (* amp (Math/sin phase))
            z (* (* 0.5 amp) (Math/cos phase))
            taper (+ 0.85 (* 0.25 (Math/sin (* 2.0 Math/PI t))))
            r (* radius taper)
            twist-angle (* twist t)]
        (dotimes [j (inc ring)]
          (let [u (/ (double j) ring)
                theta (+ (* 2.0 Math/PI u) twist-angle)
                cy (Math/cos theta)
                sy (Math/sin theta)
                px x
                py (+ y (* r cy))
                pz (+ z (* r sy))
                nx 0.0
                ny cy
                nz sy]
            (conj! vertices (float px))
            (conj! vertices (float py))
            (conj! vertices (float pz))
            (conj! vertices (float nx))
            (conj! vertices (float ny))
            (conj! vertices (float nz))))))
    (dotimes [i segments]
      (dotimes [j ring]
        (let [row-a (* i (inc ring))
              row-b (* (inc i) (inc ring))
              a (+ row-a j)
              b (+ row-b j)
              c (+ row-b (inc j))
              d (+ row-a (inc j))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))}))

(defn- axis-line-vertices
  [len]
  (float-array
   [;; X 轴（红色）
    0.0 0.0 0.0   1.0 0.0 0.0
    len 0.0 0.0   1.0 0.0 0.0
    ;; Y 轴（绿色）
    0.0 0.0 0.0   0.0 1.0 0.0
    0.0 len 0.0   0.0 1.0 0.0
    ;; Z 轴（蓝色）
    0.0 0.0 0.0   0.0 0.0 1.0
    0.0 0.0 len   0.0 0.0 1.0]))

(defn- axis-arrow-vertices
  [len arrow-len arrow-r]
  (let [x-tip [len 0.0 0.0]
        x-base [[(- len arrow-len) arrow-r arrow-r]
                [(- len arrow-len) (- arrow-r) arrow-r]
                [(- len arrow-len) (- arrow-r) (- arrow-r)]
                [(- len arrow-len) arrow-r (- arrow-r)]]
        y-tip [0.0 len 0.0]
        y-base [[arrow-r (- len arrow-len) arrow-r]
                [(- arrow-r) (- len arrow-len) arrow-r]
                [(- arrow-r) (- len arrow-len) (- arrow-r)]
                [arrow-r (- len arrow-len) (- arrow-r)]]
        z-tip [0.0 0.0 len]
        z-base [[arrow-r arrow-r (- len arrow-len)]
                [(- arrow-r) arrow-r (- len arrow-len)]
                [(- arrow-r) (- arrow-r) (- len arrow-len)]
                [arrow-r (- arrow-r) (- len arrow-len)]]
        red [1.0 0.0 0.0]
        green [0.0 1.0 0.0]
        blue [0.0 0.0 1.0]
        tri (fn [tip base color]
              [[tip color] [(nth base 0) color] [(nth base 1) color]
               [tip color] [(nth base 1) color] [(nth base 2) color]
               [tip color] [(nth base 2) color] [(nth base 3) color]
               [tip color] [(nth base 3) color] [(nth base 0) color]])]
    (float-array
     (flatten
      (concat (tri x-tip x-base red)
              (tri y-tip y-base green)
              (tri z-tip z-base blue))))))

(defn- init-resources!
  []
  (let [program (core/create-program @vs-source @fs-source)
        {:keys [vao vbo ebo index-count]} (create-quad)
        tex (create-texture 256 256)
        tex-loc (GL20/glGetUniformLocation program "texture1")
        axis-program (core/create-program axis-vs-source axis-fs-source)
        point-program (core/create-program point-vs-source point-fs-source)
        overlay-program (core/create-program overlay-vs-source overlay-fs-source)
        {:keys [length arrow-length arrow-radius]} @axis-style
        {line-vao :vao line-vbo :vbo line-count :count}
        (build-axis-vao (axis-line-vertices length))
        {arrow-vao :vao arrow-vbo :vbo arrow-count :count}
        (build-axis-vao (axis-arrow-vertices length arrow-length arrow-radius))
        ;; Cube resources
        cube-program (core/create-program cube-vs-source cube-fs-source)
        {point-vao :vao point-vbo :vbo} (build-point-vao)
        {overlay-vao :vao overlay-vbo :vbo} (build-overlay-vao)
        {mesh-vao :vao mesh-vbo :vbo mesh-ebo :ebo} (build-mesh-vao)
        {spring-vao :vao spring-vbo :vbo} (build-spring-vao)
        {cube-vao :vao cube-vbo :vbo cube-count :count} (create-cube-vao)]
    (GL20/glUseProgram program)
    (when (<= 0 tex-loc)
      (GL20/glUniform1i tex-loc 0))
    (swap! state assoc
           :program program
           :vao vao
           :vbo vbo
           :ebo ebo
           :tex tex
           :index-count index-count
           :axis-program axis-program
           :axis-line-vao line-vao
           :axis-line-vbo line-vbo
           :axis-line-count line-count
           :axis-arrow-vao arrow-vao
           :axis-arrow-vbo arrow-vbo
           :axis-arrow-count arrow-count
           ;; 立方体资源
           :cube-program cube-program
           :cube-vao cube-vao
           :cube-vbo cube-vbo
           :cube-vertex-count cube-count
           ;; 点云资源
           :point-program point-program
           :point-vao point-vao
           :point-vbo point-vbo
           :point-count 0
           :mesh-vao mesh-vao
           :mesh-vbo mesh-vbo
           :mesh-ebo mesh-ebo
           :mesh-index-count 0
           :spring-vao spring-vao
           :spring-vbo spring-vbo
           :spring-count 0
           :overlay-program overlay-program
           :overlay-vao overlay-vao
           :overlay-vbo overlay-vbo)
    ;; 使用一个默认立方体初始化
    (when (empty? @cubes)
      (reset! cubes [{:id (swap! next-cube-id inc)
                      :pos [0.0 0.0 0.0]
                      :rot [0.0 0.0 0.0]
                      :scale [1.0 1.0 1.0]
                      :color [0.8 0.4 0.2]}]))))

(defn- rebuild-axes!
  []
  (let [{:keys [axis-line-vao axis-line-vbo axis-arrow-vao axis-arrow-vbo]} @state
        {:keys [length arrow-length arrow-radius]} @axis-style
        {line-vao :vao line-vbo :vbo line-count :count}
        (build-axis-vao (axis-line-vertices length))
        {arrow-vao :vao arrow-vbo :vbo arrow-count :count}
        (build-axis-vao (axis-arrow-vertices length arrow-length arrow-radius))]
    (delete-if-positive axis-line-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive axis-line-vao #(GL30/glDeleteVertexArrays %))
    (delete-if-positive axis-arrow-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive axis-arrow-vao #(GL30/glDeleteVertexArrays %))
    (swap! state assoc
           :axis-line-vao line-vao
           :axis-line-vbo line-vbo
           :axis-line-count line-count
           :axis-arrow-vao arrow-vao
           :axis-arrow-vbo arrow-vbo
           :axis-arrow-count arrow-count)))

(defn- cleanup-resources!
  []
  (let [{:keys [program vao vbo ebo tex axis-program
                axis-line-vao axis-line-vbo axis-arrow-vao axis-arrow-vbo
                cube-program cube-vao cube-vbo
                point-program point-vao point-vbo
                overlay-program overlay-vao overlay-vbo
                mesh-vao mesh-vbo mesh-ebo
                spring-vao spring-vbo]} @state]
    (delete-if-positive program #(GL20/glDeleteProgram %))
    (delete-if-positive vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive ebo #(GL15/glDeleteBuffers %))
    (delete-if-positive vao #(GL30/glDeleteVertexArrays %))
    (delete-if-positive tex #(GL11/glDeleteTextures %))
    (delete-if-positive axis-program #(GL20/glDeleteProgram %))
    (delete-if-positive axis-line-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive axis-line-vao #(GL30/glDeleteVertexArrays %))
    (delete-if-positive axis-arrow-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive axis-arrow-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理立方体资源
    (delete-if-positive cube-program #(GL20/glDeleteProgram %))
    (delete-if-positive cube-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive cube-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理点云资源
    (delete-if-positive point-program #(GL20/glDeleteProgram %))
    (delete-if-positive point-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive point-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理网格资源
    (delete-if-positive mesh-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive mesh-ebo #(GL15/glDeleteBuffers %))
    (delete-if-positive mesh-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理弹簧线段资源
    (delete-if-positive spring-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive spring-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理覆盖层资源
    (delete-if-positive overlay-program #(GL20/glDeleteProgram %))
    (delete-if-positive overlay-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive overlay-vao #(GL30/glDeleteVertexArrays %)))
  (reset! state {:program 0 :vao 0 :vbo 0 :ebo 0 :tex 0 :index-count 0
                 :axis-program 0
                 :axis-line-vao 0 :axis-line-vbo 0 :axis-line-count 0
                 :axis-arrow-vao 0 :axis-arrow-vbo 0 :axis-arrow-count 0
                 :cube-program 0 :cube-vao 0 :cube-vbo 0 :cube-vertex-count 0
                 :point-program 0 :point-vao 0 :point-vbo 0 :point-count 0
                 :mesh-vao 0 :mesh-vbo 0 :mesh-ebo 0 :mesh-index-count 0
                 :spring-vao 0 :spring-vbo 0 :spring-count 0
                 :overlay-program 0 :overlay-vao 0 :overlay-vbo 0}))

(defn- default-render
  [ctx]
  (let [{:keys [program vao tex index-count axis-program
                axis-line-vao axis-line-count axis-arrow-vao axis-arrow-count
                cube-program cube-vao cube-vertex-count
                point-program point-vao point-count
                mesh-vao mesh-index-count
                spring-vao spring-count]} (:resources ctx)
        {:keys [yaw pitch]} (:camera ctx)
        {:keys [fb-width fb-height]} (:input ctx)
        [r g b a] @clear-color]
    (GL11/glEnable GL11/GL_DEPTH_TEST)
    (GL11/glPointSize 8.0)
    (GL11/glClearColor r g b a)
    (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

    ;; 通用矩阵
    (let [aspect (/ (float fb-width) (float fb-height))
          projection (doto (Matrix4f.)
                       (.identity)
                       (.perspective (float (Math/toRadians 45.0)) aspect 0.1 100.0))
          view (doto (Matrix4f.)
                 (.identity)
                 (.translate 0.0 0.0 -3.0)
                 (.rotateX (float pitch))
                 (.rotateY (float yaw)))
          mat-buf (BufferUtils/createFloatBuffer 16)
          axis-mvp-loc (GL20/glGetUniformLocation axis-program "mvp")
          line-width (float (:line-width @axis-style))
          ;; Cube shader locations
          cube-proj-loc (GL20/glGetUniformLocation cube-program "projection")
          cube-view-loc (GL20/glGetUniformLocation cube-program "view")
          cube-model-loc (GL20/glGetUniformLocation cube-program "model")
          cube-color-loc (GL20/glGetUniformLocation cube-program "uColor")
          cube-light-loc (GL20/glGetUniformLocation cube-program "uLightPos")
          cube-viewpos-loc (GL20/glGetUniformLocation cube-program "uViewPos")]

      ;; Draw axes
      (let [mvp (doto (Matrix4f. projection) (.mul view))]
        (GL20/glUseProgram axis-program)
        (upload-mat! mvp mat-buf axis-mvp-loc)
        (GL11/glLineWidth line-width)
        (GL30/glBindVertexArray axis-line-vao)
        (GL11/glDrawArrays GL11/GL_LINES 0 axis-line-count)
        (GL30/glBindVertexArray axis-arrow-vao)
        (GL11/glDrawArrays GL11/GL_TRIANGLES 0 axis-arrow-count)
        (GL11/glLineWidth 1.0))

      ;; Draw spring lines
      (when (pos? spring-count)
        (let [mvp (doto (Matrix4f. projection) (.mul view))]
          (GL20/glUseProgram axis-program)
          (upload-mat! mvp mat-buf axis-mvp-loc)
          (GL11/glLineWidth 1.5)
          (GL30/glBindVertexArray spring-vao)
          (GL11/glDrawArrays GL11/GL_LINES 0 spring-count)
          (GL11/glLineWidth 1.0)))

      ;; Draw point cloud
      (when (pos? point-count)
        (let [mvp (doto (Matrix4f. projection) (.mul view))
              point-mvp-loc (GL20/glGetUniformLocation point-program "mvp")]
          (GL20/glUseProgram point-program)
          (upload-mat! mvp mat-buf point-mvp-loc)
          (GL30/glBindVertexArray point-vao)
          (GL11/glDrawArrays GL11/GL_POINTS 0 point-count)))

      ;; Draw cubes
      (GL20/glUseProgram cube-program)
      (when (<= 0 cube-proj-loc)
        (upload-mat! projection mat-buf cube-proj-loc))
      (when (<= 0 cube-view-loc)
        (upload-mat! view mat-buf cube-view-loc))
      (when (<= 0 cube-light-loc)
        (GL20/glUniform3f cube-light-loc 2.0 2.0 2.0))
      (when (<= 0 cube-viewpos-loc)
        (GL20/glUniform3f cube-viewpos-loc 0.0 0.0 3.0))

      (GL30/glBindVertexArray cube-vao)
      (doseq [{:keys [pos rot scale color]} @cubes]
        (let [model (compose-transform pos rot scale)]
          (when (<= 0 cube-model-loc)
            (upload-mat! model mat-buf cube-model-loc))
          (when (<= 0 cube-color-loc)
            (GL20/glUniform3f cube-color-loc (float (nth color 0)) (float (nth color 1)) (float (nth color 2))))
          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 cube-vertex-count)))

      ;; Draw rig segments
      (when (seq (:segments @rig-state))
        (doseq [{:keys [model color]} (:segments @rig-state)]
          (when (<= 0 cube-model-loc)
            (upload-mat! model mat-buf cube-model-loc))
          (when (<= 0 cube-color-loc)
            (GL20/glUniform3f cube-color-loc (float (nth color 0)) (float (nth color 1)) (float (nth color 2))))
          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 cube-vertex-count)))

      ;; Draw generic mesh (e.g. sphere)
      (when (pos? mesh-index-count)
        (let [{:keys [pos rot scale color]} @mesh-style
              model (compose-transform pos rot scale)]
          (when (<= 0 cube-model-loc)
            (upload-mat! model mat-buf cube-model-loc))
          (when (<= 0 cube-color-loc)
            (GL20/glUniform3f cube-color-loc (float (nth color 0)) (float (nth color 1)) (float (nth color 2))))
          (GL30/glBindVertexArray mesh-vao)
          (GL11/glDrawElements GL11/GL_TRIANGLES mesh-index-count GL11/GL_UNSIGNED_INT 0)))

      ;; 绘制原始四边形（在立方体后方，作为参考）
      (let [mvp (doto (Matrix4f.)
                  (.identity)
                  (.translate 0.0 0.0 -2.0)
                  (.rotateX (float pitch))
                  (.rotateY (float yaw)))
            quad-mvp-loc (GL20/glGetUniformLocation program "mvp")
            mat-buf (BufferUtils/createFloatBuffer 16)]
        (GL20/glUseProgram program)
        (upload-mat! mvp mat-buf quad-mvp-loc)
        (GL13/glActiveTexture GL13/GL_TEXTURE0)
        (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
        (GL30/glBindVertexArray vao)
        (GL11/glDrawElements GL11/GL_TRIANGLES index-count GL11/GL_UNSIGNED_INT 0)))))

(defn- ensure-default-scenes!
  []
  (when-not (contains? @scenes :baseline)
    (register-scene!
     :baseline
     {:init (fn [_] nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}))
  (when-not (contains? @scenes :geometry)
    (register-scene!
     :geometry
     {:init (fn [_]
              (reset! clear-color [0.05 0.05 0.08 1.0])
              (reset! cubes [{:id (swap! next-cube-id inc)
                              :pos [-1.0 0.0 0.0]
                              :rot [0.0 0.0 0.0]
                              :scale [0.6 0.6 0.6]
                              :color [0.9 0.4 0.2]}
                             {:id (swap! next-cube-id inc)
                              :pos [0.0 0.0 0.0]
                              :rot [0.0 0.0 0.0]
                              :scale [0.8 0.8 0.8]
                              :color [0.2 0.8 0.4]}
                             {:id (swap! next-cube-id inc)
                              :pos [0.8 -0.6 0.3]
                              :rot [0.0 0.0 0.0]
                              :scale [0.5 0.5 0.5]
                              :color [0.2 0.6 0.9]}])
              (let [points (sphere-point-cloud 120 0.7)
                    colors (repeatedly (count points) #(color3 (rand) (rand) (rand)))]
                (set-point-cloud! points :colors colors))
              (let [{:keys [vertices indices]} (uv-sphere 24 36)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      deg (* 30.0 dt)]
                  (swap! cubes
                         (fn [cs]
                           (mapv (fn [c]
                                   (update c :rot (fn [[rx ry rz]]
                                                    [(+ rx deg) (+ ry (* 0.6 deg)) rz])))
                                 cs))))
                scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)})))

(register-scene!
 :rig
 {:init (fn [_]
          (reset! clear-color [0.03 0.03 0.06 1.0])
          (reset! cubes [])
          (clear-point-cloud!)
          (clear-mesh!)
          (reset! rig-state {:segments []})
          {:t 0.0})
  :update (fn [{:keys [time]} scene-state]
            (let [t (+ (:t scene-state) (:dt time))
                  root-rot [0.0 (* 25.0 (Math/sin t)) 0.0]
                  upper-rot [(* 35.0 (Math/sin (* 1.2 t))) 0.0 0.0]
                  lower-rot [(* -50.0 (Math/sin (* 1.6 t))) 0.0 0.0]
                  root (compose-transform [0.0 -0.1 0.0] root-rot [0.32 0.9 0.32])
                  upper (chain-transform root [0.0 0.9 0.0] upper-rot [0.30 0.85 0.30])
                  lower (chain-transform upper [0.0 0.85 0.0] lower-rot [0.28 0.8 0.28])]
              (reset! rig-state
                      {:segments [{:model root :color [0.9 0.4 0.2]}
                                  {:model upper :color [0.2 0.8 0.4]}
                                  {:model lower :color [0.2 0.6 0.9]}]})
              (assoc scene-state :t t)))
  :render (fn [ctx _] (default-render ctx))
  :cleanup (fn [_ _] nil)})

(register-scene!
 :sweep
 {:init (fn [_]
          (reset! clear-color [0.02 0.02 0.05 1.0])
          (reset! cubes [])
          (clear-point-cloud!)
          (reset! rig-state {:segments []})
          (let [{:keys [vertices indices]} (sweep-mesh)]
            (set-mesh! vertices indices))
          {:t 0.0})
  :update (fn [{:keys [time]} scene-state]
            (let [t (+ (:t scene-state) (:dt time))
                  twist (* 90.0 (Math/sin (* 0.6 t)))
                  {:keys [vertices indices]}
                  (sweep-mesh :segments 90
                              :ring 18
                              :length 3.2
                              :radius 0.22
                              :amp 0.4
                              :freq 1.3
                              :twist twist)]
              (set-mesh! vertices indices)
              (assoc scene-state :t t)))
  :render (fn [ctx _] (default-render ctx))
  :cleanup (fn [_ _] nil)})

(register-scene!
 :particles
 {:init (fn [_]
          (reset! clear-color [0.01 0.01 0.04 1.0])
          (reset! cubes [])
          (reset! rig-state {:segments []})
          (swap! state assoc :mesh-index-count 0)
          (let [particles (init-particles 400 1.6)
                {:keys [points colors]} (particles->points particles)]
            (upload-point-cloud! points colors)
            {:t 0.0 :particles particles}))
  :update (fn [{:keys [time]} scene-state]
            (let [dt (:dt time)
                  t (+ (:t scene-state) dt)
                  particles (update-particles (:particles scene-state) dt t)
                  {:keys [points colors]} (particles->points particles)]
              (upload-point-cloud! points colors)
              (assoc scene-state :t t :particles particles)))
  :render (fn [ctx _] (default-render ctx))
  :cleanup (fn [_ _] nil)})

(register-scene!
 :sdf
 {:init (fn [_]
          (reset! clear-color [0.02 0.02 0.05 1.0])
          (reset! cubes [])
          (clear-point-cloud!)
          (reset! rig-state {:segments []})
          (set-mesh-style! {:pos [0.0 0.0 0.0]
                            :rot [0.0 0.0 0.0]
                            :scale [1.0 1.0 1.0]
                            :color [0.7 0.7 0.95]})
          (let [{:keys [vertices indices]}
                (marching-tetrahedra sdf-metaballs {:min [-1.4 -1.1 -1.1]
                                                    :max [1.4 1.1 1.1]
                                                    :res 36
                                                    :t 0.0})]
            (set-mesh! vertices indices))
          {:t 0.0 :acc 0.0})
  :update (fn [{:keys [time]} scene-state]
            (let [scene-state (or scene-state {:t 0.0 :acc 0.0})
                  dt (:dt time)
                  t (+ (:t scene-state) dt)
                  acc (+ (:acc scene-state) dt)]
              (if (>= acc 0.2)
                (let [{:keys [vertices indices]}
                      (marching-tetrahedra sdf-metaballs {:min [-1.4 -1.1 -1.1]
                                                          :max [1.4 1.1 1.1]
                                                          :res 36
                                                          :t t})]
                  (set-mesh! vertices indices)
                  (assoc scene-state :t t :acc 0.0))
                (assoc scene-state :t t :acc acc))))
  :render (fn [ctx _] (default-render ctx))
  :cleanup (fn [_ _] nil)})

(register-scene!
 :spring
 {:init (fn [_]
          (reset! clear-color [0.02 0.02 0.05 1.0])
          (reset! cubes [])
          (clear-point-cloud!)
          (reset! rig-state {:segments []})
          (swap! state assoc :mesh-index-count 0)
          (let [{:keys [nodes springs]} (make-spring-grid 10 10 0.22)
                points (mapv :pos nodes)
                colors (mapv (fn [{:keys [fixed?]}]
                               (if fixed? [1.0 0.9 0.2] [0.8 0.8 0.9]))
                             nodes)
                lines (spring-lines nodes springs)]
            (upload-point-cloud! points colors)
            (upload-spring-lines! lines)
            {:nodes nodes :springs springs}))
  :update (fn [{:keys [time]} scene-state]
            (let [dt (:dt time)
                  nodes (update-spring-grid (:nodes scene-state) (:springs scene-state) dt)
                  points (mapv :pos nodes)
                  colors (mapv (fn [{:keys [fixed?]}]
                                 (if fixed? [1.0 0.9 0.2] [0.8 0.8 0.9]))
                               nodes)
                  lines (spring-lines nodes (:springs scene-state))]
              (upload-point-cloud! points colors)
              (upload-spring-lines! lines)
              (assoc scene-state :nodes nodes)))
  :render (fn [ctx _] (default-render ctx))
  :cleanup (fn [_ _] nil)})

(register-scene!
 :ecosystem
 {:init (fn [_]
          (reset! clear-color [0.02 0.02 0.05 1.0])
          (reset! rig-state {:segments []})
          (clear-point-cloud!)
          (clear-mesh!)
          (clear-spring-lines!)
          (let [plants (repeatedly 30 (fn []
                                        {:pos [(rand-range -1.2 1.2) 0.0 (rand-range -1.0 1.0)]
                                         :energy 1.0}))
                herbivores (repeatedly 14 (fn []
                                            {:pos [(rand-range -1.4 1.4) 0.0 (rand-range -1.2 1.2)]
                                             :vel [0.0 0.0 0.0]
                                             :energy 1.2}))
                predators (repeatedly 6 (fn []
                                          {:pos [(rand-range -1.4 1.4) 0.0 (rand-range -1.2 1.2)]
                                           :vel [0.0 0.0 0.0]
                                           :energy 1.6}))]
            (reset! cubes [])
            {:plants (vec plants)
             :herbivores (vec herbivores)
             :predators (vec predators)
             :t 0.0}))
  :update (fn [{:keys [time]} scene-state]
            (let [dt (:dt time)
                  t (+ (:t scene-state) dt)
                  dist2 (fn [a b]
                          (let [[ax ay az] a
                                [bx by bz] b
                                dx (- bx ax)
                                dy (- by ay)
                                dz (- bz az)]
                            (+ (* dx dx) (* dy dy) (* dz dz))))
                  steer (fn [pos target speed]
                          (let [[px py pz] pos
                                [tx ty tz] target
                                dx (- tx px)
                                dy (- ty py)
                                dz (- tz pz)
                                dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-6))
                                s (min speed (/ speed dist))]
                            [(* dx s) (* dy s) (* dz s)]))
                  plants (:plants scene-state)
                  herbivores
                  (mapv
                   (fn [{:keys [pos energy] :as h}]
                     (let [food (when (seq plants)
                                  (apply min-key #(dist2 pos (:pos %)) plants))
                           jitter [(rand-range -0.35 0.35) 0.0 (rand-range -0.35 0.35)]
                           vel (if food
                                 (let [d (dist2 pos (:pos food))]
                                   (if (< d 0.04)
                                     jitter
                                     (steer pos (:pos food) 1.2)))
                                 jitter)
                           [vx vy vz] vel
                           [px py pz] pos
                           npos [(+ px (* vx dt)) 0.0 (+ pz (* vz dt))]
                           energy (- energy (* 0.08 dt))
                           eaten? (and food (< (dist2 npos (:pos food)) 0.03))]
                       (cond-> (assoc h :pos npos :vel vel :energy energy)
                         eaten? (update :energy + 0.6))))
                   (:herbivores scene-state))
                  eaten-plants
                  (set (keep (fn [{:keys [pos energy]}]
                               (when (> energy 1.5) pos))
                             herbivores))
                  plants
                  (vec (remove (fn [{:keys [pos]}]
                                 (some #(<= (dist2 pos %) 0.03) eaten-plants))
                               plants))
                  herbivores
                  (vec (remove #(<= (:energy %) 0.0) herbivores))
                  born-herbivores
                  (mapcat (fn [{:keys [pos energy]}]
                            (when (> energy 2.2)
                              [{:pos [(+ (first pos) (rand-range -0.1 0.1))
                                      0.0
                                      (+ (nth pos 2) (rand-range -0.1 0.1))]
                                :vel [0.0 0.0 0.0]
                                :energy 1.0}]))
                          herbivores)
                  herbivores
                  (vec (concat (mapv (fn [h]
                                       (if (> (:energy h) 2.2)
                                         (assoc h :energy 1.2)
                                         h))
                                     herbivores)
                               born-herbivores))
                  predators
                  (mapv
                   (fn [{:keys [pos energy] :as p}]
                     (let [target (when (seq herbivores)
                                    (apply min-key #(dist2 pos (:pos %)) herbivores))
                           jitter [(rand-range -0.25 0.25) 0.0 (rand-range -0.25 0.25)]
                           vel (if target
                                 (let [d (dist2 pos (:pos target))]
                                   (if (< d 0.06)
                                     jitter
                                     (steer pos (:pos target) 1.6)))
                                 jitter)
                           [vx vy vz] vel
                           [px py pz] pos
                           npos [(+ px (* vx dt)) 0.0 (+ pz (* vz dt))]
                           energy (- energy (* 0.12 dt))
                           eaten? (and target (< (dist2 npos (:pos target)) 0.04))]
                       (cond-> (assoc p :pos npos :vel vel :energy energy)
                         eaten? (update :energy + 0.8))))
                   (:predators scene-state))
                  eaten-herbivores
                  (set (keep (fn [{:keys [pos energy]}]
                               (when (> energy 2.0) pos))
                             predators))
                  herbivores
                  (vec (remove (fn [{:keys [pos]}]
                                 (some #(<= (dist2 pos %) 0.04) eaten-herbivores))
                               herbivores))
                  predators
                  (vec (remove #(<= (:energy %) 0.0) predators))
                  born-predators
                  (mapcat (fn [{:keys [pos energy]}]
                            (when (> energy 2.6)
                              [{:pos [(+ (first pos) (rand-range -0.1 0.1))
                                      0.0
                                      (+ (nth pos 2) (rand-range -0.1 0.1))]
                                :vel [0.0 0.0 0.0]
                                :energy 1.4}]))
                          predators)
                  predators
                  (vec (concat (mapv (fn [p]
                                       (if (> (:energy p) 2.6)
                                         (assoc p :energy 1.6)
                                         p))
                                     predators)
                               born-predators))
                  plants
                  (vec (concat plants
                               (repeatedly (max 0 (- 30 (count plants)))
                                           (fn []
                                             {:pos [(rand-range -1.2 1.2) 0.0 (rand-range -1.0 1.0)]
                                              :energy 1.0}))))]
              (reset! cubes
                      (vec
                       (concat
                        (map (fn [{:keys [pos]}]
                               {:id (swap! next-cube-id inc)
                                :pos pos :rot [0.0 0.0 0.0] :scale [0.12 0.12 0.12]
                                :color [0.2 0.9 0.3]})
                             plants)
                        (map (fn [{:keys [pos]}]
                               {:id (swap! next-cube-id inc)
                                :pos pos :rot [0.0 0.0 0.0] :scale [0.16 0.16 0.16]
                                :color [0.9 0.9 0.2]})
                             herbivores)
                        (map (fn [{:keys [pos]}]
                               {:id (swap! next-cube-id inc)
                                :pos pos :rot [0.0 0.0 0.0] :scale [0.2 0.2 0.2]
                                :color [0.9 0.2 0.2]})
                             predators))))
              (assoc scene-state
                     :plants plants
                     :herbivores herbivores
                     :predators predators
                     :t t)))
  :render (fn [ctx _] (default-render ctx))
  :cleanup (fn [_ _] nil)})

;; ---- 热重载 API（面向 REPL） ----------------------------------------

(defn set-clear-color!
  [r g b a]
  (reset! clear-color [r g b a]))

(defn add-cube!
  "动态添加一个新立方体。返回该立方体的 id。
  可选参数：
    :pos [x y z] - 位置（默认 [0 0 0]）
    :rot [rx ry rz] - 旋转角度，单位为度（默认 [0 0 0]）
    :scale [sx sy sz] - 缩放系数（默认 [1 1 1]）
    :color [r g b] - 颜色 RGB 值 0-1（默认随机颜色）"
  [& {:keys [pos rot scale color]
      :or {pos [0.0 0.0 0.0]
           rot [0.0 0.0 0.0]
           scale [1.0 1.0 1.0]
           color [(+ 0.3 (rand 0.7))
                  (+ 0.3 (rand 0.7))
                  (+ 0.3 (rand 0.7))]}}]
  (let [id (swap! next-cube-id inc)
        cube {:id id :pos pos :rot rot :scale scale :color color}]
    (swap! cubes conj cube)
    id))

(defn remove-cube!
  "根据 id 移除立方体。如果找到并移除成功则返回 true。"
  [id]
  (let [before (count @cubes)]
    (swap! cubes (fn [cs] (vec (remove #(= (:id %) id) cs))))
    (< (count @cubes) before)))

(defn remove-all-cubes!
  "移除所有立方体。"
  []
  (reset! cubes []))

(defn update-cube!
  "根据 id 更新立方体的属性。支持的键：:pos、:rot、:scale、:color。"
  [id & {:as updates}]
  (swap! cubes
         (fn [cs]
           (mapv (fn [c]
                   (if (= (:id c) id)
                     (merge c (select-keys updates [:pos :rot :scale :color]))
                     c))
                 cs))))

(defn list-cubes
  "返回包含所有立方体及其 id 的向量。"
  []
  @cubes)

(defn cube-count
  "返回立方体的数量。"
  []
  (count @cubes))

(defn set-key-handler!
  "处理器函数签名：(fn [window key action])"
  [f]
  (reset! key-handler f))

(defn set-axis-style!
  "更新坐标轴样式。支持的键：:line-width、:length、:arrow-length、:arrow-radius。
  几何变化会在 GL 线程上应用。"
  [style]
  (let [keys-to-update (select-keys style [:line-width :length :arrow-length :arrow-radius])
        geometry? (some #(contains? keys-to-update %) [:length :arrow-length :arrow-radius])]
    (swap! axis-style merge keys-to-update)
    (if geometry?
      (enqueue! rebuild-axes!)
      (let [reply (async/promise-chan)]
        (async/put! reply {:ok :no-op})
        reply))))

(defn set-render!
  "替换渲染函数。函数签名：(fn [state])，会覆盖当前场景的渲染。"
  [f]
  (reset! render-fn f))

(def ^:private demo-timeline
  [{:scene :geometry :duration 6.0}
   {:scene :rig :duration 6.0}
   {:scene :sweep :duration 6.0}
   {:scene :particles :duration 6.0}
   {:scene :sdf :duration 8.0}
   {:scene :spring :duration 8.0}
   {:scene :ecosystem :duration 10.0}])

(defn start-demo!
  "启动默认演示时间线。"
  []
  (set-timeline! demo-timeline)
  (start-timeline!))

(defn stop-demo!
  "停止默认演示时间线。"
  []
  (stop-timeline!))

(defn register-demo-commands!
  "注册默认快捷键：
  1-7 切换场景，0 启动演示，P 暂停/恢复。"
  []
  (register-command! GLFW/GLFW_KEY_1 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :geometry)))
  (register-command! GLFW/GLFW_KEY_2 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :rig)))
  (register-command! GLFW/GLFW_KEY_3 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :sweep)))
  (register-command! GLFW/GLFW_KEY_4 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :particles)))
  (register-command! GLFW/GLFW_KEY_5 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :sdf)))
  (register-command! GLFW/GLFW_KEY_6 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :spring)))
  (register-command! GLFW/GLFW_KEY_7 GLFW/GLFW_PRESS (fn [_ _ _] (set-scene! :ecosystem)))
  (register-command! GLFW/GLFW_KEY_0 GLFW/GLFW_PRESS (fn [_ _ _] (start-demo!)))
  (register-command! GLFW/GLFW_KEY_P GLFW/GLFW_PRESS
                     (fn [_ _ _]
                       (if (get-in @app [:flags :paused?])
                         (resume!)
                         (pause!))))
  :ok)

(defn reload-shaders!
  "在 GL 线程上重新加载着色器。接受新的顶点/片段着色器源码字符串。
  传入 nil 表示保持现有的不变。

  示例：
  (async/<!! (reload-shaders! nil new-frag-src)))"
  [new-vs new-fs]
  (when new-vs (reset! vs-source new-vs))
  (when new-fs (reset! fs-source new-fs))
  (enqueue!
   (fn []
     (let [old (:program @state)
           program (core/create-program @vs-source @fs-source)
           tex-loc (GL20/glGetUniformLocation program "texture1")]
       (when (pos? old) (GL20/glDeleteProgram old))
       (GL20/glUseProgram program)
       (when (<= 0 tex-loc)
         (GL20/glUniform1i tex-loc 0))
       (swap! state assoc :program program)
       program))))

(defn update-vertices!
  "在 GL 线程上替换四边形数据。传入顶点数据的 float-array 和索引的 int-array。
  保持相同的属性布局。"
  [vertices indices]
  (enqueue!
   (fn []
     (let [{:keys [vbo ebo]} @state
           vbuf (BufferUtils/createFloatBuffer (alength vertices))
           ibuf (BufferUtils/createIntBuffer (alength indices))]
       (.put vbuf vertices)
       (.flip vbuf)
       (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
       (GL15/glBufferData GL15/GL_ARRAY_BUFFER vbuf GL15/GL_STATIC_DRAW)
       (.put ibuf indices)
       (.flip ibuf)
       (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER ebo)
       (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER ibuf GL15/GL_STATIC_DRAW)
       (swap! state assoc :index-count (alength indices))))))

(defn set-mesh!
  "设置通用网格数据。vertices 为包含位置与法线的 float-array，indices 为 int-array。"
  [vertices indices]
  (enqueue!
   (fn []
     (let [{:keys [mesh-vao mesh-vbo mesh-ebo]} @state
           vbuf (BufferUtils/createFloatBuffer (alength vertices))
           ibuf (BufferUtils/createIntBuffer (alength indices))]
       (.put vbuf vertices)
       (.flip vbuf)
       (GL30/glBindVertexArray mesh-vao)
       (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER mesh-vbo)
       (GL15/glBufferData GL15/GL_ARRAY_BUFFER vbuf GL15/GL_DYNAMIC_DRAW)
       (.put ibuf indices)
       (.flip ibuf)
       (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER mesh-ebo)
       (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER ibuf GL15/GL_DYNAMIC_DRAW)
       (GL30/glBindVertexArray 0)
       (swap! state assoc :mesh-index-count (alength indices))))))

(defn clear-mesh!
  "清空通用网格。"
  []
  (set-mesh! (float-array []) (int-array [])))

(defn set-mesh-style!
  "设置网格的位姿与颜色。"
  [{:keys [pos rot scale color]}]
  (swap! mesh-style merge
         (select-keys {:pos pos :rot rot :scale scale :color color}
                      [:pos :rot :scale :color])))

(defn- upload-spring-lines!
  [line-data]
  (let [{:keys [spring-vbo]} @state
        line-data (float-array line-data)
        count (quot (alength line-data) 6)
        buf (BufferUtils/createFloatBuffer (alength line-data))]
    (.put buf line-data)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER spring-vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_DYNAMIC_DRAW)
    (swap! state assoc :spring-count count)))

(defn- clear-spring-lines!
  []
  (upload-spring-lines! []))

(defn- upload-point-cloud!
  [points colors]
  (let [{:keys [point-vbo]} @state
        point-count (count points)
        data (float-array (* point-count 6))]
    (dotimes [i point-count]
      (let [[x y z] (nth points i)
            [r g b] (if (< i (count colors))
                      (nth colors i)
                      [1.0 1.0 1.0])
            base (* i 6)]
        (aset-float data base (float x))
        (aset-float data (inc base) (float y))
        (aset-float data (+ base 2) (float z))
        (aset-float data (+ base 3) (float r))
        (aset-float data (+ base 4) (float g))
        (aset-float data (+ base 5) (float b))))
    (let [buf (BufferUtils/createFloatBuffer (alength data))]
      (.put buf data)
      (.flip buf)
      (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER point-vbo)
      (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_DYNAMIC_DRAW))
    (swap! state assoc :point-count point-count)))

(defn set-point-cloud!
  "设置点云数据。points 为 [[x y z] ...]，colors 为 [[r g b] ...]。
  若 colors 为空或不足，自动补白色。"
  [points & {:keys [colors]}]
  (let [points (vec points)
        colors (vec (or colors []))]
    (reset! point-cloud-state {:points points :colors colors})
    (enqueue!
     (fn []
       (upload-point-cloud! points colors)))))

(defn clear-point-cloud!
  "清空点云数据。"
  []
  (set-point-cloud! []))

;; ---- 主入口 -----------------------------------------------------------

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LWJGL - Hot Reload (experiment)")]
    (try
      (swap! app assoc :window window)
      (swap! app assoc-in [:input :win-width] width)
      (swap! app assoc-in [:input :win-height] height)
      (GL/createCapabilities)
      (core/init-viewport! window width height)
      (let [w (BufferUtils/createIntBuffer 1)
            h (BufferUtils/createIntBuffer 1)]
        (GLFW/glfwGetFramebufferSize window w h)
        (swap! app assoc-in [:input :fb-width] (.get w 0))
        (swap! app assoc-in [:input :fb-height] (.get h 0)))
      (swap! app assoc :time {:now (now-seconds)
                              :dt 0.0
                              :frame 0
                              :last (now-seconds)})
      (GLFW/glfwSetFramebufferSizeCallback
       window
       (reify GLFWFramebufferSizeCallbackI
         (invoke [_ _ w h]
           (swap! app assoc-in [:input :fb-width] w)
           (swap! app assoc-in [:input :fb-height] h)
           (GL11/glViewport 0 0 w h))))
      (GLFW/glfwSetKeyCallback
       window
       (reify GLFWKeyCallbackI
         (invoke [_ win key _ action _]
           (when-let [cmd (get @command-table [key action])]
             (cmd win key action))
           (when-let [f @key-handler]
             (f win key action)))))
      (GLFW/glfwSetMouseButtonCallback
       window
       (reify GLFWMouseButtonCallbackI
         (invoke [_ _ button action _]
           (when (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
             (swap! app assoc-in [:input :dragging?] (= action GLFW/GLFW_PRESS))))))
      (GLFW/glfwSetCursorPosCallback
       window
       (reify GLFWCursorPosCallbackI
         (invoke [_ _ xpos ypos]
           (let [{:keys [dragging? last-x last-y]} (:input @app)]
             (when (or dragging? (zero? last-x))
               (let [dx (- xpos last-x)
                     dy (- ypos last-y)]
                 (swap! app assoc-in [:input :last-x] xpos)
                 (swap! app assoc-in [:input :last-y] ypos)
                 (when dragging?
                   (swap! app update-in [:camera :yaw] + (* 0.005 dx))
                   (swap! app update-in [:camera :pitch] + (* -0.005 dy)))))))))
      (init-resources!)
      (ensure-default-scenes!)
      (apply-scene! :baseline)
      (reset! render-fn nil)
      (reset! key-handler
              (fn [win key action]
                (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                           (= action GLFW/GLFW_PRESS))
                  (GLFW/glfwSetWindowShouldClose win true))))
      (loop []
        (when-not (GLFW/glfwWindowShouldClose window)
          (drain-commands!)
          (update-time!)
          (let [dt (get-in @app [:time :dt])]
            (update-timeline! dt)
            (update-transition! dt)
            (update-scene! dt))
          (render-scene!)
          (render-transition!)
          (GLFW/glfwSwapBuffers window)
          (GLFW/glfwPollEvents)
          (recur)))
      (finally
        (cleanup-resources!)
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& args]
  (when (some #{"--nrepl"} args)
    (let [server (core/start-nrepl!)]
      (println "nREPL server started for hotreload")))
  (run-example!))

(comment
  (set-clear-color! 0.5 0.05 0.08 1.0)
  (set-axis-style! {:line-width 1.0
                    :arrow-length 0.02
                    :arrow-radius 0.02})
  (reload-shaders!
   nil
   "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
void main() {
    FragColor = vec4(1.0 - TexCoord.x, TexCoord.y, 0.2, 1.0);
}")

  ;; 替换渲染逻辑
  (set-render!
   (fn [{:keys [program vao index-count]}]
     (GL11/glClearColor 0.1 0.2 0.1 1.0)
     (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
     (GL20/glUseProgram program)
     (GL30/glBindVertexArray vao)
     (GL11/glDrawElements GL11/GL_TRIANGLES index-count GL11/GL_UNSIGNED_INT 0)))

  ;; 更新顶点（创建一个更小的四边形）
  (update-vertices!
   (float-array [0.3 0.3 0.0  1.0 1.0
                 0.3 -0.3 0.0 1.0 0.0
                 -0.3 -0.3 0.0 0.0 0.0
                 -0.3 0.3 0.0 0.0 1.0])
   (int-array [0 1 3 1 2 3]))

  (swap! axis-style assoc :arrow-radius 10.0)
  (reset! clear-color [0.1 0.5 0.5 0.9])
  (add-cube!)
  (add-cube! :pos [1.5 0 -1] :color [0.2 0.8 0.4])
  (add-cube! :pos [-0.5 -0.5 0.5] :color [0.2 0.8 0.4])
  (cube-count)
  (remove-all-cubes!))

;; REPL 示例（在窗口打开时运行）：
;;
;; (require '[lwjgl.experiment.hotreload :as hr]
;;          '[clojure.core.async :as async]
;;          :reload)
;;
;; ;; 修改清除颜色（无需 GL 调用）
;; (hr/set-clear-color! 0.05 0.05 0.08 1.0)
;;
;; ;; 调整坐标轴粗细 / 箭头大小
;; (async/<!! (hr/set-axis-style! {:line-width 4.0
;;                                :arrow-length 0.18
;;                                :arrow-radius 0.08}))
;;
;; ;; 热重载片段着色器
;; (async/<!!
;;  (hr/reload-shaders!
;;   nil
;;   "#version 330 core
;; in vec2 TexCoord;
;; out vec4 FragColor;
;; void main() {
;;     FragColor = vec4(1.0 - TexCoord.x, TexCoord.y, 0.2, 1.0);
;; }"))
;;
;; ;; 替换渲染逻辑
;; (hr/set-render!
;;  (fn [{:keys [program vao index-count]}]
;;    (GL11/glClearColor 0.1 0.1 0.1 1.0)
;;    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
;;    (GL20/glUseProgram program)
;;    (GL30/glBindVertexArray vao)
;;    (GL11/glDrawElements GL11/GL_TRIANGLES index-count GL11/GL_UNSIGNED_INT 0)))
;;
;; ;; 更新顶点（创建更小的四边形）
;; (async/<!!
;;  (hr/update-vertices!
;;   (float-array [0.3 0.3 0.0  1.0 1.0
;;                 0.3 -0.3 0.0 1.0 0.0
;;                 -0.3 -0.3 0.0 0.0 0.0
;;                 -0.3 0.3 0.0 0.0 1.0])
;;   (int-array [0 1 3 1 2 3])))
;;
;; ;; ---- 立方体示例 --------------------------------------------------------
;;
;; ;; 在原点添加一个随机颜色的立方体
;; (hr/add-cube!)
;;
;; ;; 在指定位置添加自定义颜色的立方体
;; (hr/add-cube! :pos [1.5 0.0 -1.0] :color [0.2 0.8 0.4])
;;
;; ;; 添加一个旋转并缩放的立方体
;; (hr/add-cube! :pos [-1.0 0.5 0.5]
;;               :rot [0.0 45.0 0.0]
;;               :scale [0.5 0.5 0.5]
;;               :color [0.9 0.3 0.3])
;;
;; ;; 添加一排立方体
;; (doseq [x (range -3 4)]
;;   (hr/add-cube! :pos [(* x 0.8) 0.0 0.0]
;;                 :color [(rand) (rand) (rand)]))
;;
;; ;; 列出所有立方体
;; (hr/list-cubes)
;;
;; ;; 获取立方体数量
;; (hr/cube-count)
;;
;; ;; 更新立方体（修改位置和颜色）
;; (hr/update-cube! 1 :pos [0.0 1.0 0.0] :color [1.0 1.0 0.0])
;;
;; ;; 根据 id 移除立方体
;; (hr/remove-cube! 1)
;;
;; ;; 移除所有立方体
;; (hr/remove-all-cubes!)
