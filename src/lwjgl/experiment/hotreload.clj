(ns lwjgl.experiment.hotreload
  (:gen-class)
  (:require [clojure.core.async :as async]
            [lwjgl.core :as core]
            [lwjgl.experiment.kons9.geometry :as kgeom]
            [lwjgl.experiment.kons9.input :as kinput]
            [lwjgl.experiment.kons9.math :as kmath]
            [lwjgl.experiment.kons9.particles :as kpart]
            [lwjgl.experiment.kons9.runtime :as krt]
            [lwjgl.experiment.kons9.scenes :as kscenes]
            [lwjgl.experiment.kons9.sdf :as ksdf]
            [lwjgl.experiment.kons9.spring :as kspring])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWMouseButtonCallbackI GLFWScrollCallbackI
                           GLFWWindowSizeCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30 GL32)
           (org.joml Matrix4f)))

;; ---- 运行时状态 ---------------------------------------------------------

(defonce cmd-chan (async/chan 128))
(defonce render-fn (atom nil))
(defonce clear-color (atom [0.2 0.3 0.3 1.0]))
(defonce axis-style
  (atom {:length 1.0
         :arrow-length 0.12
         :arrow-radius 0.05
         :line-width 2.0}))
(defonce grid-style
  (atom {:size 6.0
         :step 0.5
         :major-step 1.0
         :minor-color [0.18 0.18 0.24]
         :major-color [0.28 0.28 0.34]
         :line-width 1.0}))
(defonce app
  (atom {:window 0
         :time {:now 0.0 :dt 0.0 :frame 0 :last 0.0}
         :scene {:key :baseline :state nil :since 0.0}
         :timeline {:enabled? false :items [] :index 0 :elapsed 0.0}
         :transition {:active? false :alpha 0.0 :duration 0.6}
         :camera {:yaw 0.0 :pitch 0.0 :distance 3.0}
         :input {:fb-width 800 :fb-height 600
                 :win-width 800 :win-height 600
                 :dragging? false :last-x 0.0 :last-y 0.0}
         :flags {:paused? false
                 :manual-play? false
                 :playing? true}}))

;; 立方体状态 - 每个立方体包含 :pos [x y z], :rot [rx ry rz], :scale [sx sy sz], :color [r g b]
(defonce cubes (atom []))
(defonce next-cube-id (atom 0))

(defonce scenes (atom {}))
(defonce point-cloud-state (atom {:points [] :colors []}))
(defonce sprite-state (atom {:points [] :colors [] :sizes []}))
(defonce sprite-style
  (atom {:size 16.0
         :alpha 0.75
         :softness 0.15}))
(defonce rig-state (atom {:segments []}))
(defonce mesh-style
  (atom {:pos [1.6 0.0 0.0]
         :rot [0.0 0.0 0.0]
         :scale [0.8 0.8 0.8]
         :color [0.2 0.6 0.9]}))
(defonce wireframe-overlay
  (atom {:enabled? false
         :line-width 1.2
         :color [0.08 0.08 0.1]}))

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
         ;; 网格平面
         :grid-vao 0
         :grid-vbo 0
         :grid-count 0
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
         ;; 精灵渲染资源
         :sprite-program 0
         :sprite-vao 0
         :sprite-vbo 0
         :sprite-count 0
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

(def ^:private sprite-vs-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 aColor;
layout (location = 2) in float aSize;
uniform mat4 mvp;
out vec4 vColor;
void main() {
    vColor = aColor;
    gl_Position = mvp * vec4(aPos, 1.0);
    gl_PointSize = aSize;
}")

(def ^:private sprite-fs-source
  "#version 330 core
in vec4 vColor;
uniform float uSoftness;
out vec4 FragColor;
void main() {
    vec2 uv = gl_PointCoord * 2.0 - 1.0;
    float d = length(uv);
    if (d > 1.0) discard;
    float edge = max(0.001, uSoftness);
    float alpha = 1.0 - smoothstep(1.0 - edge, 1.0, d);
    FragColor = vec4(vColor.rgb, vColor.a * alpha);
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
         set-sprites!
         clear-sprites!
         set-sprite-style!
         set-mesh!
         set-mesh-style!
         set-wireframe-overlay!
         set-grid-style!
         set-line-segments!
         clear-line-segments!
         clear-spring-lines!
         clear-mesh!)

(defn enqueue!
  "将一个函数加入队列在 OpenGL/主线程上运行. 返回一个 promise-chan,
  它会返回 {:ok value} 或 {:err throwable}. "
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
  (krt/current-scene-def app scenes))

(defn- apply-scene!
  [scene-key]
  (krt/apply-scene! app scenes scene-context scene-key))

(defn register-scene!
  "注册一个场景, scene 需要包含 :init/:update/:render/:cleanup 中的任意组合. "
  [scene-key scene]
  (krt/register-scene! scenes scene-key scene))

(defn list-scenes
  "返回已注册的场景 key 列表. "
  []
  (krt/list-scenes scenes))

(defn current-scene
  "返回当前场景 key. "
  []
  (krt/current-scene app))

(defn set-scene!
  "切换到指定场景. 该操作在 GL 线程中执行. "
  [scene-key]
  (ensure-default-scenes!)
  (enqueue! #(apply-scene! scene-key)))

(defn set-timeline!
  "设置时间线. items 形如 {:scene :foo :duration 5.0} 的向量. "
  [items]
  (krt/set-timeline! app items))

(defn start-timeline!
  "启动时间线并切换到首个场景. "
  []
  (ensure-default-scenes!)
  (enqueue! #(krt/start-timeline! app scenes scene-context)))

(defn stop-timeline!
  "停止时间线推进. "
  []
  (krt/stop-timeline! app))

(defn set-transition!
  "设置过场时长(秒). "
  [duration]
  (krt/set-transition! app duration))

(defn trigger-transition!
  "手动触发一次过场遮罩. "
  []
  (krt/trigger-transition! app))

(defn pause!
  "暂停场景更新. "
  []
  (krt/pause! app))

(defn resume!
  "恢复场景更新. "
  []
  (krt/resume! app))

(defn set-manual-play!
  "设置手动播放模式. 开启后需要按住空格才会推进时间. "
  [enabled?]
  (swap! app assoc-in [:flags :manual-play?] (boolean enabled?))
  (when-not enabled?
    (swap! app assoc-in [:flags :playing?] true))
  :ok)

(defn reset-time!
  "重置时间轴与场景计时. "
  []
  (let [now (now-seconds)]
    (swap! app assoc :time {:now now :dt 0.0 :frame 0 :last now})
    (swap! app assoc-in [:scene :since] 0.0))
  :ok)

(defn reset-scene!
  "重新初始化当前场景并重置时间. "
  []
  (reset-time!)
  (enqueue! #(apply-scene! (current-scene))))

(defn register-command!
  "注册键盘命令. action 可以是 GLFW/GLFW_PRESS 等. "
  [key action f]
  (kinput/register-command! key action f))

(defn- update-time!
  []
  (let [now (now-seconds)
        last (get-in @app [:time :last])
        dt (max 0.0 (- now last))
        paused? (get-in @app [:flags :paused?])
        manual? (get-in @app [:flags :manual-play?])
        playing? (get-in @app [:flags :playing?])
        dt (cond
             paused? 0.0
             (and manual? (not playing?)) 0.0
             :else dt)
        frame (get-in @app [:time :frame])
        frame (if (pos? dt) (inc frame) frame)]
    (swap! app assoc :time {:now now
                            :dt dt
                            :frame frame
                            :last now})))

(defn- update-scene!
  [dt]
  (krt/update-scene! app scenes scene-context dt))

(defn- update-timeline!
  [dt]
  (krt/update-timeline! app scenes scene-context dt))

(defn- update-transition!
  [dt]
  (krt/update-transition! app dt))

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

(defn vec3
  "创建三维向量. "
  [x y z]
  (kmath/vec3 x y z))

(defn color3
  "创建颜色, 自动限制在 0-1 范围内. "
  [r g b]
  (kmath/color3 r g b))

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.rewind buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn- sprite-color
  "补齐精灵颜色并保证有 alpha. "
  [color default-alpha]
  (cond
    (and (vector? color) (= 4 (count color))) color
    (and (vector? color) (= 3 (count color))) (conj color default-alpha)
    (and (sequential? color) (= 4 (count color))) (vec color)
    (and (sequential? color) (= 3 (count color))) (vec (concat color [default-alpha]))
    :else [1.0 1.0 1.0 default-alpha]))

(defn- sprite-depth
  "使用视图矩阵计算相机空间的 Z 值, 用于透明排序. "
  [^Matrix4f view [x y z]]
  (let [m02 (.m02 view)
        m12 (.m12 view)
        m22 (.m22 view)
        m32 (.m32 view)]
    (+ (* m02 x) (* m12 y) (* m22 z) m32)))

(defn- upload-sprites!
  "根据当前相机排序并上传精灵数据, 返回精灵数量. "
  [^Matrix4f view]
  (let [{:keys [points colors sizes]} @sprite-state
        points (vec points)
        count (count points)]
    (if (zero? count)
      (do
        (swap! state assoc :sprite-count 0)
        0)
      (let [{:keys [alpha size]} @sprite-style
            colors (vec (or colors []))
            sizes (vec (or sizes []))
            order (sort-by :z (map-indexed (fn [i p] {:i i :z (sprite-depth view p)}) points))
            buf (BufferUtils/createFloatBuffer (* count 8))]
        (doseq [{:keys [i]} order]
          (let [[x y z] (nth points i)
                [r g b a] (sprite-color (get colors i) alpha)
                s (float (or (get sizes i) size))]
            (.put buf (float x))
            (.put buf (float y))
            (.put buf (float z))
            (.put buf (float r))
            (.put buf (float g))
            (.put buf (float b))
            (.put buf (float a))
            (.put buf s)))
        (.flip buf)
        (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER (:sprite-vbo @state))
        (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_DYNAMIC_DRAW)
        (swap! state assoc :sprite-count count)
        count))))

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

(defn- build-grid-vao
  []
  (let [{:keys [size step major-step minor-color major-color]} @grid-style
        size (double size)
        step (double step)
        major-step (double major-step)
        coords (range (- size) (+ size 1.0e-6) step)
        data (transient [])
        major? (fn [v]
                 (let [r (Math/abs (rem v major-step))]
                   (< r (* 0.5 step))))]
    (doseq [z coords]
      (let [[r g b] (if (major? z) major-color minor-color)]
        (conj! data (float (- size)))
        (conj! data 0.0)
        (conj! data (float z))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))
        (conj! data (float size))
        (conj! data 0.0)
        (conj! data (float z))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))))
    (doseq [x coords]
      (let [[r g b] (if (major? x) major-color minor-color)]
        (conj! data (float x))
        (conj! data 0.0)
        (conj! data (float (- size)))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))
        (conj! data (float x))
        (conj! data 0.0)
        (conj! data (float size))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))))
    (build-axis-vao (float-array (persistent! data)))))

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

(defn- build-sprite-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        stride (* 8 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER 0 GL15/GL_DYNAMIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 4 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL20/glVertexAttribPointer 2 1 GL11/GL_FLOAT false stride (* 7 Float/BYTES))
    (GL20/glEnableVertexAttribArray 2)
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
  "生成 UV 球体网格, 返回 {:vertices float-array :indices int-array}.
  参数:
    lat-segs - 纬向分段数
    lon-segs - 经向分段数"
  [lat-segs lon-segs]
  (kgeom/uv-sphere lat-segs lon-segs))

(defn make-point-cloud
  "创建点云数据. 传入 points 与可选 colors.
  points 为 [[x y z] ...].
  colors 为 [[r g b] ...], 若不足则使用白色. "
  [points & {:keys [colors]}]
  (kgeom/make-point-cloud points :colors colors))

(defn sphere-point-cloud
  "生成球面点云. "
  [count radius]
  (kgeom/sphere-point-cloud count radius))

(defn- compose-transform
  [pos rot scale]
  (kmath/compose-transform pos rot scale))

(defn- chain-transform
  [parent pos rot scale]
  (kmath/chain-transform parent pos rot scale))

(defn- rand-range
  [a b]
  (kmath/rand-range a b))

(defn- init-particles
  [count radius]
  (kpart/init-particles count radius))

(defn- update-particles
  [particles dt t]
  (kpart/update-particles particles dt t))

(defn- particles->points
  [particles]
  (kpart/particles->points particles))

(defn- make-spring-grid
  [rows cols spacing]
  (kspring/make-spring-grid rows cols spacing))

(defn- update-spring-grid
  [nodes springs dt]
  (kspring/update-spring-grid nodes springs dt))

(defn- spring-lines
  [nodes springs]
  (kspring/spring-lines nodes springs))

(defn- sdf-metaballs
  [x y z t]
  (ksdf/sdf-metaballs x y z t))

(defn marching-tetrahedra
  "使用四面体分割的 marching 算法生成网格. "
  [f {:keys [min max res t]}]
  (ksdf/marching-tetrahedra f {:min min :max max :res res :t t}))

(defn sweep-mesh
  "生成沿 X 轴扫掠的管状网格. 返回 {:vertices float-array :indices int-array}. "
  [& opts]
  (apply kgeom/sweep-mesh opts))

(defn- axis-line-vertices
  [len]
  (float-array
   [;; X 轴(红色)
    0.0 0.0 0.0   1.0 0.0 0.0
    len 0.0 0.0   1.0 0.0 0.0
    ;; Y 轴(绿色)
    0.0 0.0 0.0   0.0 1.0 0.0
    0.0 len 0.0   0.0 1.0 0.0
    ;; Z 轴(蓝色)
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
        sprite-program (core/create-program sprite-vs-source sprite-fs-source)
        overlay-program (core/create-program overlay-vs-source overlay-fs-source)
        {:keys [length arrow-length arrow-radius]} @axis-style
        {line-vao :vao line-vbo :vbo line-count :count}
        (build-axis-vao (axis-line-vertices length))
        {arrow-vao :vao arrow-vbo :vbo arrow-count :count}
        (build-axis-vao (axis-arrow-vertices length arrow-length arrow-radius))
        {grid-vao :vao grid-vbo :vbo grid-count :count} (build-grid-vao)
        ;; Cube resources
        cube-program (core/create-program cube-vs-source cube-fs-source)
        {point-vao :vao point-vbo :vbo} (build-point-vao)
        {sprite-vao :vao sprite-vbo :vbo} (build-sprite-vao)
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
           :grid-vao grid-vao
           :grid-vbo grid-vbo
           :grid-count grid-count
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
           ;; 精灵资源
           :sprite-program sprite-program
           :sprite-vao sprite-vao
           :sprite-vbo sprite-vbo
           :sprite-count 0
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

(defn- rebuild-grid!
  []
  (let [{:keys [grid-vao grid-vbo]} @state
        {new-vao :vao new-vbo :vbo new-count :count} (build-grid-vao)]
    (delete-if-positive grid-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive grid-vao #(GL30/glDeleteVertexArrays %))
    (swap! state assoc
           :grid-vao new-vao
           :grid-vbo new-vbo
           :grid-count new-count)))

(defn- cleanup-resources!
  []
  (let [{:keys [program vao vbo ebo tex axis-program
                axis-line-vao axis-line-vbo axis-arrow-vao axis-arrow-vbo
                grid-vao grid-vbo
                cube-program cube-vao cube-vbo
                point-program point-vao point-vbo
                sprite-program sprite-vao sprite-vbo
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
    (delete-if-positive grid-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive grid-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理立方体资源
    (delete-if-positive cube-program #(GL20/glDeleteProgram %))
    (delete-if-positive cube-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive cube-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理点云资源
    (delete-if-positive point-program #(GL20/glDeleteProgram %))
    (delete-if-positive point-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive point-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理精灵资源
    (delete-if-positive sprite-program #(GL20/glDeleteProgram %))
    (delete-if-positive sprite-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive sprite-vao #(GL30/glDeleteVertexArrays %))
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
                 :sprite-program 0 :sprite-vao 0 :sprite-vbo 0 :sprite-count 0
                 :mesh-vao 0 :mesh-vbo 0 :mesh-ebo 0 :mesh-index-count 0
                 :spring-vao 0 :spring-vbo 0 :spring-count 0
                 :overlay-program 0 :overlay-vao 0 :overlay-vbo 0}))

(defn- default-render
  [ctx]
  (let [{:keys [program vao tex index-count axis-program
                axis-line-vao axis-line-count axis-arrow-vao axis-arrow-count
                grid-vao grid-count
                cube-program cube-vao cube-vertex-count
                point-program point-vao point-count
                sprite-program sprite-vao
                mesh-vao mesh-index-count
                spring-vao spring-count]} (:resources ctx)
        {:keys [yaw pitch distance]} (:camera ctx)
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
                 (.translate 0.0 0.0 (float (- (double (or distance 3.0)))))
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
          cube-viewpos-loc (GL20/glGetUniformLocation cube-program "uViewPos")
          sprite-mvp-loc (GL20/glGetUniformLocation sprite-program "mvp")
          sprite-soft-loc (GL20/glGetUniformLocation sprite-program "uSoftness")]

      ;; Draw grid
      (let [mvp (doto (Matrix4f. projection) (.mul view))
            grid-width (:line-width @grid-style)]
        (when (pos? grid-count)
          (GL20/glUseProgram axis-program)
          (upload-mat! mvp mat-buf axis-mvp-loc)
          (GL11/glLineWidth (float grid-width))
          (GL30/glBindVertexArray grid-vao)
          (GL11/glDrawArrays GL11/GL_LINES 0 grid-count)
          (GL11/glLineWidth 1.0)))

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

      ;; Draw sprites (透明排序后绘制)
      (let [sprite-count (upload-sprites! view)]
        (when (pos? sprite-count)
          (let [mvp (doto (Matrix4f. projection) (.mul view))
                softness (float (:softness @sprite-style))]
            (GL11/glEnable GL32/GL_PROGRAM_POINT_SIZE)
            (GL11/glEnable GL11/GL_BLEND)
            (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
            (GL11/glDepthMask false)
            (GL20/glUseProgram sprite-program)
            (upload-mat! mvp mat-buf sprite-mvp-loc)
            (when (<= 0 sprite-soft-loc)
              (GL20/glUniform1f sprite-soft-loc softness))
            (GL30/glBindVertexArray sprite-vao)
            (GL11/glDrawArrays GL11/GL_POINTS 0 sprite-count)
            (GL30/glBindVertexArray 0)
            (GL11/glDepthMask true)
            (GL11/glDisable GL11/GL_BLEND))))

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
          (GL11/glDrawElements GL11/GL_TRIANGLES mesh-index-count GL11/GL_UNSIGNED_INT 0)
          (when (:enabled? @wireframe-overlay)
            (let [{:keys [line-width color]} @wireframe-overlay
                  [wr wg wb] color]
              (GL11/glEnable GL11/GL_POLYGON_OFFSET_LINE)
              (GL11/glPolygonOffset -1.0 -1.0)
              (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK GL11/GL_LINE)
              (GL11/glLineWidth (float line-width))
              (when (<= 0 cube-model-loc)
                (upload-mat! model mat-buf cube-model-loc))
              (when (<= 0 cube-color-loc)
                (GL20/glUniform3f cube-color-loc (float wr) (float wg) (float wb)))
              (GL11/glDrawElements GL11/GL_TRIANGLES mesh-index-count GL11/GL_UNSIGNED_INT 0)
              (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK GL11/GL_FILL)
              (GL11/glDisable GL11/GL_POLYGON_OFFSET_LINE)
              (GL11/glLineWidth 1.0)))))

      ;; 绘制原始四边形(在立方体后方, 作为参考)
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

(defn render-default
  "公开的默认渲染入口. "
  [ctx]
  (default-render ctx))

(defn- scene-api
  "组装场景使用的回调接口. "
  []
  {:set-clear-color! (fn [color] (reset! clear-color color))
   :set-cubes! (fn [cs] (reset! cubes (vec cs)))
   :update-cubes! (fn [f] (swap! cubes f))
   :next-cube-id! (fn [] (swap! next-cube-id inc))
   :set-point-cloud! set-point-cloud!
   :clear-point-cloud! clear-point-cloud!
   :set-sprites! set-sprites!
   :clear-sprites! clear-sprites!
   :set-sprite-style! set-sprite-style!
   :upload-point-cloud! upload-point-cloud!
   :set-mesh! set-mesh!
   :clear-mesh! clear-mesh!
   :set-mesh-style! set-mesh-style!
   :set-wireframe-overlay! set-wireframe-overlay!
   :set-mesh-index-count! (fn [n] (swap! state assoc :mesh-index-count n))
   :upload-spring-lines! upload-spring-lines!
   :clear-spring-lines! clear-spring-lines!
   :set-line-segments! set-line-segments!
   :clear-line-segments! clear-line-segments!
   :set-rig-segments! (fn [segments] (reset! rig-state {:segments segments}))
   :clear-rig! (fn [] (reset! rig-state {:segments []}))
   :default-render default-render})

(defn- ensure-default-scenes!
  []
  (doseq [[scene-key scene] (kscenes/default-scenes (scene-api))]
    (when-not (contains? @scenes scene-key)
      (register-scene! scene-key scene))))

;; ---- 热重载 API(面向 REPL) ----------------------------------------

(defn set-clear-color!
  [r g b a]
  (reset! clear-color [r g b a]))

(defn add-cube!
  "动态添加一个新立方体. 返回该立方体的 id.
  可选参数:
    :pos [x y z] - 位置(默认 [0 0 0])
    :rot [rx ry rz] - 旋转角度, 单位为度(默认 [0 0 0])
    :scale [sx sy sz] - 缩放系数(默认 [1 1 1])
    :color [r g b] - 颜色 RGB 值 0-1(默认随机颜色)"
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
  "根据 id 移除立方体. 如果找到并移除成功则返回 true. "
  [id]
  (let [before (count @cubes)]
    (swap! cubes (fn [cs] (vec (remove #(= (:id %) id) cs))))
    (< (count @cubes) before)))

(defn remove-all-cubes!
  "移除所有立方体. "
  []
  (reset! cubes []))

(defn update-cube!
  "根据 id 更新立方体的属性. 支持的键: :pos, :rot, :scale, :color. "
  [id & {:as updates}]
  (swap! cubes
         (fn [cs]
           (mapv (fn [c]
                   (if (= (:id c) id)
                     (merge c (select-keys updates [:pos :rot :scale :color]))
                     c))
                 cs))))

(defn list-cubes
  "返回包含所有立方体及其 id 的向量. "
  []
  @cubes)

(defn cube-count
  "返回立方体的数量. "
  []
  (count @cubes))

(defn set-key-handler!
  "处理器函数签名: (fn [window key action])"
  [f]
  (kinput/set-key-handler! f))

(defn set-camera-distance!
  "设置相机距离(缩放)."
  [distance]
  (swap! app assoc-in [:camera :distance] (double distance))
  :ok)

(defn set-axis-style!
  "更新坐标轴样式. 支持的键: :line-width, :length, :arrow-length, :arrow-radius.
  几何变化会在 GL 线程上应用. "
  [style]
  (let [keys-to-update (select-keys style [:line-width :length :arrow-length :arrow-radius])
        geometry? (some #(contains? keys-to-update %) [:length :arrow-length :arrow-radius])]
    (swap! axis-style merge keys-to-update)
    (if geometry?
      (enqueue! rebuild-axes!)
      (let [reply (async/promise-chan)]
        (async/put! reply {:ok :no-op})
        reply))))

(defn set-grid-style!
  "更新网格样式. 支持的键: :size, :step, :major-step, :minor-color, :major-color, :line-width.
  几何变化会在 GL 线程上应用. "
  [style]
  (let [keys-to-update (select-keys style [:size :step :major-step :minor-color :major-color :line-width])
        geometry? (some #(contains? keys-to-update %) [:size :step :major-step :minor-color :major-color])]
    (swap! grid-style merge keys-to-update)
    (if geometry?
      (enqueue! rebuild-grid!)
      (let [reply (async/promise-chan)]
        (async/put! reply {:ok :no-op})
        reply))))

(defn set-render!
  "替换渲染函数. 函数签名: (fn [state]), 会覆盖当前场景的渲染. "
  [f]
  (reset! render-fn f))

(def ^:private demo-timeline
  [{:scene :geometry :duration 6.0}
   {:scene :rig :duration 6.0}
   {:scene :sweep :duration 6.0}
   {:scene :heightfield :duration 6.0}
   {:scene :resource-growth :duration 6.0}
   {:scene :heightfield-sweep :duration 6.0}
   {:scene :surface-particles :duration 6.0}
   {:scene :polyhedron-refine :duration 6.0}
   {:scene :polyhedron-fractal :duration 6.0}
   {:scene :cut-cube :duration 6.0}
   {:scene :box :duration 6.0}
   {:scene :isosurface-points :duration 8.0}
   {:scene :isosurface-curves :duration 8.0}
   {:scene :isosurface-particles :duration 8.0}
   {:scene :spring :duration 8.0}
   {:scene :spring-isosurface :duration 8.0}
   {:scene :sdf-demo :duration 10.0}
   {:scene :sweep-live :duration 10.0}
   {:scene :ecosystem :duration 10.0}])

(defn start-demo!
  "启动默认演示时间线. "
  []
  (set-timeline! demo-timeline)
  (start-timeline!))

(defn stop-demo!
  "停止默认演示时间线. "
  []
  (stop-timeline!))

(defn register-demo-commands!
  "注册默认快捷键:
  1-7 切换场景, 0 启动演示, P 暂停/恢复. "
  []
  (kinput/register-demo-commands!
   (fn [key action cmd]
     (case cmd
       :demo (register-command! key action (fn [_ _ _] (start-demo!)))
       :pause (register-command! key action (fn [_ _ _]
                                              (if (get-in @app [:flags :paused?])
                                                (resume!)
                                                (pause!))))
       (register-command! key action (fn [_ _ _] (set-scene! cmd)))))))

(defn reload-shaders!
  "在 GL 线程上重新加载着色器. 接受新的顶点/片段着色器源码字符串.
  传入 nil 表示保持现有的不变.

  示例:
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
  "在 GL 线程上替换四边形数据. 传入顶点数据的 float-array 和索引的 int-array.
  保持相同的属性布局. "
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
  "设置通用网格数据. vertices 为包含位置与法线的 float-array, indices 为 int-array. "
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
  "清空通用网格. "
  []
  (set-mesh! (float-array []) (int-array [])))

(defn set-mesh-style!
  "设置网格的位姿与颜色. "
  [{:keys [pos rot scale color]}]
  (swap! mesh-style merge
         (select-keys {:pos pos :rot rot :scale scale :color color}
                      [:pos :rot :scale :color])))

(defn set-wireframe-overlay!
  "设置网格线框覆盖层。支持的键：:enabled?、:line-width、:color。"
  [style]
  (swap! wireframe-overlay merge
         (select-keys style [:enabled? :line-width :color]))
  :ok)

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

(defn set-line-segments!
  "设置通用线段数据。line-data 为 [x y z r g b ...] 序列。"
  [line-data]
  (enqueue!
   (fn []
     (upload-spring-lines! line-data))))

(defn clear-line-segments!
  "清空通用线段数据。"
  []
  (set-line-segments! []))

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
  "设置点云数据. points 为 [[x y z] ...], colors 为 [[r g b] ...].
  若 colors 为空或不足, 自动补白色. "
  [points & {:keys [colors]}]
  (let [points (vec points)
        colors (vec (or colors []))]
    (reset! point-cloud-state {:points points :colors colors})
    (enqueue!
     (fn []
       (upload-point-cloud! points colors)))))

(defn clear-point-cloud!
  "清空点云数据. "
  []
  (set-point-cloud! []))

(defn set-sprites!
  "设置精灵数据. points 为 [[x y z] ...], colors 为 [[r g b] 或 [r g b a] ...],
  sizes 为每个精灵的像素尺寸. 若缺省则使用默认样式. "
  [points & {:keys [colors sizes]}]
  (let [points (vec points)
        colors (vec (or colors []))
        sizes (vec (or sizes []))]
    (reset! sprite-state {:points points :colors colors :sizes sizes})
    :ok))

(defn clear-sprites!
  "清空精灵数据. "
  []
  (reset! sprite-state {:points [] :colors [] :sizes []})
  :ok)

(defn set-sprite-style!
  "设置精灵样式. 支持的键: :size, :alpha, :softness. "
  [style]
  (swap! sprite-style merge
         (select-keys style [:size :alpha :softness]))
  :ok)

(defn set-rig-segments!
  "设置机械臂/实例段列表. "
  [segments]
  (reset! rig-state {:segments (vec segments)}))

(defn clear-rig!
  "清空机械臂/实例段. "
  []
  (reset! rig-state {:segments []}))

;; ---- 主入口 -----------------------------------------------------------

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LWJGL - Hot Reload (experiment)")]
    (try
      (swap! app assoc :window window)
      (let [w (BufferUtils/createIntBuffer 1)
            h (BufferUtils/createIntBuffer 1)]
        (GLFW/glfwGetWindowSize window w h)
        (swap! app assoc-in [:input :win-width] (.get w 0))
        (swap! app assoc-in [:input :win-height] (.get h 0)))
      (let [w (BufferUtils/createIntBuffer 1)
            h (BufferUtils/createIntBuffer 1)]
        (GLFW/glfwGetFramebufferSize window w h)
        (swap! app assoc-in [:input :fb-width] (.get w 0))
        (swap! app assoc-in [:input :fb-height] (.get h 0)))
      (GL/createCapabilities)
      ;; 这里强制使用 framebuffer 尺寸初始化 viewport, 避免 HiDPI 下比例错误
      (core/init-viewport! window (get-in @app [:input :fb-width]) (get-in @app [:input :fb-height]))
      (swap! app assoc :time {:now (now-seconds)
                              :dt 0.0
                              :frame 0
                              :last (now-seconds)})
      (GLFW/glfwSetWindowSizeCallback
       window
       (reify GLFWWindowSizeCallbackI
         (invoke [_ _ w h]
           (swap! app assoc-in [:input :win-width] w)
           (swap! app assoc-in [:input :win-height] h))))
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
           (kinput/handle-key win key action))))
      (GLFW/glfwSetMouseButtonCallback
       window
       (reify GLFWMouseButtonCallbackI
         (invoke [_ win button action _]
           (when (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
             (if (= action GLFW/GLFW_PRESS)
               (let [xbuf (BufferUtils/createDoubleBuffer 1)
                     ybuf (BufferUtils/createDoubleBuffer 1)]
                 (GLFW/glfwGetCursorPos win xbuf ybuf)
                 (swap! app assoc-in [:input :dragging?] true)
                 (swap! app assoc-in [:input :last-x] (.get xbuf 0))
                 (swap! app assoc-in [:input :last-y] (.get ybuf 0)))
               (swap! app assoc-in [:input :dragging?] false))))))
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
      (GLFW/glfwSetScrollCallback
       window
       (reify GLFWScrollCallbackI
         (invoke [_ _ _ yoffset]
           (swap! app update-in [:camera :distance]
                  (fn [d]
                    (let [d (double (or d 3.0))
                          next (- d (* 0.35 (double yoffset)))]
                      (-> next
                          (max 0.6)
                          (min 12.0))))))))
      (init-resources!)
      (ensure-default-scenes!)
      (register-demo-commands!)
      (register-command! GLFW/GLFW_KEY_M GLFW/GLFW_PRESS
                         (fn [_ _ _]
                           (set-manual-play!
                            (not (get-in @app [:flags :manual-play?])))))
      (register-command! GLFW/GLFW_KEY_SPACE GLFW/GLFW_PRESS
                         (fn [_ _ _]
                           (when (get-in @app [:flags :manual-play?])
                             (swap! app assoc-in [:flags :playing?] true))))
      (register-command! GLFW/GLFW_KEY_SPACE GLFW/GLFW_RELEASE
                         (fn [_ _ _]
                           (when (get-in @app [:flags :manual-play?])
                             (swap! app assoc-in [:flags :playing?] false))))
      (register-command! GLFW/GLFW_KEY_D GLFW/GLFW_PRESS
                         (fn [_ _ _]
                           (reset-scene!)))
      (apply-scene! :baseline)
      (reset! render-fn nil)
      (set-key-handler!
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

  ;; 更新顶点(创建一个更小的四边形)
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

;; REPL 示例(在窗口打开时运行):
;;
;; (require '[lwjgl.experiment.hotreload :as hr]
;;          '[clojure.core.async :as async]
;;          :reload)
;;
;; ;; 修改清除颜色(无需 GL 调用)
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
;; ;; 更新顶点(创建更小的四边形)
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
;; ;; 更新立方体(修改位置和颜色)
;; (hr/update-cube! 1 :pos [0.0 1.0 0.0] :color [1.0 1.0 0.0])
;;
;; ;; 根据 id 移除立方体
;; (hr/remove-cube! 1)
;;
;; ;; 移除所有立方体
;; (hr/remove-all-cubes!)
