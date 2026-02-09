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
            [lwjgl.experiment.kons9.spring :as kspring]
            [lwjgl.experiment.kons9.ui :as kui])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWMouseButtonCallbackI GLFWScrollCallbackI
                           GLFWWindowSizeCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30 GL32)
           (org.joml Matrix4f)))

;; ---- 运行时状态 ---------------------------------------------------------

(defonce cmd-chan (async/chan 128))
(defonce render-fn (atom nil))
(defonce clear-color (atom [0.94 0.95 0.97 1.0]))
(defonce axis-style
  (atom {:length 1.0
         :arrow-length 0.12
         :arrow-radius 0.05
         :shaft-radius 0.003}))
(defonce grid-style
  (atom {:size 6.0
         :step 0.5
         :major-step 1.0
         :y -1.0
         :minor-color [0.18 0.18 0.24]
         :major-color [0.28 0.28 0.34]
         :line-width 1.0}))
(defonce lighting-style
  (atom {:ambient [0.14 0.14 0.16]
         :specular-strength 0.35
         :shininess 32.0
         :lights [{:id :key
                   :pos [2.2 2.0 1.8]
                   :color [1.0 0.98 0.95]
                   :intensity 1.0
                   :enabled? true}
                  {:id :fill
                   :pos [-2.4 1.4 1.6]
                   :color [0.85 0.9 1.0]
                   :intensity 0.55
                   :enabled? true}
                  {:id :rim
                   :pos [0.0 2.2 -2.6]
                   :color [1.0 1.0 1.0]
                   :intensity 0.35
                   :enabled? true}
                  {:id :head
                   :pos [0.0 0.0 3.0]
                   :color [1.0 1.0 1.0]
                   :intensity 0.25
                   :enabled? false
                   :follow-camera? true}]}))
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
                 :playing? true
                 :ui-interactive? false}}))

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

(defonce mesh-data
  (atom {:base nil
         :smooth nil}))

(defonce display-state
  (atom {:display-filled? true
         :display-wireframe? false
         :display-points? false
         :smooth-shading? false
         :backface-cull? false
         :show-grid? true
         :show-axes? true
         :show-cubes? true
         :show-mesh? true
         :show-points? true
         :show-lines? true
         :lighting? true}))

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
         :overlay-vbo 0
         ;; UI 覆盖层
         :ui-rect-program 0
         :ui-rect-vao 0
         :ui-rect-vbo 0
         :ui-rect-buffer nil
         :ui-rect-viewport-loc -1
         :ui-rect-color-loc -1
         :ui-text-program 0
         :ui-text-vao 0
         :ui-text-vbo 0
         :ui-text-tex 0
         :ui-text-tex-w 0
         :ui-text-tex-h 0
         :ui-text-bytes nil
         :ui-text-buffer nil
         :ui-text-vertex-buffer nil
         :ui-text-viewport-loc -1
         :ui-text-color-loc -1}))

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

(def ^:private ui-rect-vs-source
  "#version 330 core
layout (location = 0) in vec2 aPos;
uniform vec2 uViewport;
void main() {
    vec2 ndc = vec2((aPos.x / uViewport.x) * 2.0 - 1.0,
                    1.0 - (aPos.y / uViewport.y) * 2.0);
    gl_Position = vec4(ndc, 0.0, 1.0);
}")

(def ^:private ui-rect-fs-source
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
uniform vec3 uViewPos;
uniform vec3 uAmbient;
uniform float uSpecularStrength;
uniform float uShininess;
uniform int uLightCount;
uniform vec3 uLightPos[4];
uniform vec3 uLightColor[4];
uniform float uLightIntensity[4];
out vec4 FragColor;
void main() {
    vec3 norm = normalize(vNormal);
    vec3 viewDir = normalize(uViewPos - vPos);
    vec3 lighting = uAmbient * uColor;
    for (int i = 0; i < 4; i++) {
        if (i >= uLightCount) break;
        vec3 lightDir = normalize(uLightPos[i] - vPos);
        float diff = max(dot(norm, lightDir), 0.0);
        vec3 reflectDir = reflect(-lightDir, norm);
        float spec = pow(max(dot(viewDir, reflectDir), 0.0), uShininess);
        vec3 lightCol = uLightColor[i] * uLightIntensity[i];
        vec3 diffuse = diff * lightCol * uColor;
        vec3 specular = uSpecularStrength * spec * lightCol;
        lighting += diffuse + specular;
    }
    FragColor = vec4(lighting, 1.0);
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
         interactive-mode?
         set-interactive-mode!
         info-lines
         apply-mesh-shading!
         set-line-segments!
         clear-line-segments!
         clear-spring-lines!
         clear-mesh!
         clear-rig!)

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
            :axis-style @axis-style
            :lighting @lighting-style}
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

(declare update-ui-rect-buffer! ensure-ui-texture!)

(defn- render-ui-rects!
  [rects win-w win-h]
  (when (seq rects)
    (let [{:keys [ui-rect-program ui-rect-vao ui-rect-vbo ui-rect-buffer
                  ui-rect-viewport-loc ui-rect-color-loc]} @state]
      (GL11/glDisable GL11/GL_DEPTH_TEST)
      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
      (GL20/glUseProgram ui-rect-program)
      (when (<= 0 ui-rect-viewport-loc)
        (GL20/glUniform2f ui-rect-viewport-loc (float win-w) (float win-h)))
      (GL30/glBindVertexArray ui-rect-vao)
      (doseq [{:keys [x y w h color]} rects]
        (let [[r g b a] (or color [1.0 1.0 1.0 1.0])]
          (when (<= 0 ui-rect-color-loc)
            (GL20/glUniform4f ui-rect-color-loc (float r) (float g) (float b) (float a)))
          (update-ui-rect-buffer! ui-rect-buffer (float x) (float y) (float w) (float h))
          (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER ui-rect-vbo)
          (GL15/glBufferSubData GL15/GL_ARRAY_BUFFER 0 ui-rect-buffer)
          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 6)))
      (GL30/glBindVertexArray 0)
      (GL11/glDisable GL11/GL_BLEND)
      (GL11/glEnable GL11/GL_DEPTH_TEST))))

(defn- render-ui-text!
  [texts win-w win-h text-color]
  (when (seq texts)
    (ensure-ui-texture! win-w win-h)
    (let [{:keys [ui-text-program ui-text-vao ui-text-tex ui-text-tex-w ui-text-tex-h
                  ui-text-bytes ui-text-buffer ui-text-viewport-loc ui-text-color-loc]} @state
          [r g b] (or text-color [0.1 0.1 0.1])]
      (core/clear-text-buffer! ui-text-bytes)
      (doseq [{:keys [x y text]} texts]
        (core/draw-string! ui-text-bytes ui-text-tex-w ui-text-tex-h (int x) (int y) (str text)))
      (.clear ui-text-buffer)
      (.put ui-text-buffer ui-text-bytes 0 (* ui-text-tex-w ui-text-tex-h))
      (.flip ui-text-buffer)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D ui-text-tex)
      (GL11/glTexSubImage2D GL11/GL_TEXTURE_2D 0 0 0 ui-text-tex-w ui-text-tex-h
                            GL11/GL_RED GL11/GL_UNSIGNED_BYTE ui-text-buffer)
      (GL11/glDisable GL11/GL_DEPTH_TEST)
      (GL11/glEnable GL11/GL_BLEND)
      (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
      (GL20/glUseProgram ui-text-program)
      (when (<= 0 ui-text-viewport-loc)
        (GL20/glUniform2f ui-text-viewport-loc (float win-w) (float win-h)))
      (when (<= 0 ui-text-color-loc)
        (GL20/glUniform3f ui-text-color-loc (float r) (float g) (float b)))
      (GL13/glActiveTexture GL13/GL_TEXTURE0)
      (GL11/glBindTexture GL11/GL_TEXTURE_2D ui-text-tex)
      (GL30/glBindVertexArray ui-text-vao)
      (GL11/glDrawArrays GL11/GL_TRIANGLES 0 6)
      (GL30/glBindVertexArray 0)
      (GL11/glDisable GL11/GL_BLEND)
      (GL11/glEnable GL11/GL_DEPTH_TEST))))

(defn- render-ui!
  []
  (when-not (interactive-mode?)
    (let [{:keys [win-width win-height]} (:input @app)]
      (when (and (pos? win-width) (pos? win-height))
        (when (kui/info-visible?)
          (kui/set-info-lines! (info-lines)))
        (let [{:keys [rects texts text-color]} (kui/draw-commands win-width win-height)]
          (render-ui-rects! rects win-width win-height)
          (render-ui-text! texts win-width win-height text-color))))))

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
  (let [{:keys [size step major-step minor-color major-color y]} @grid-style
        size (double size)
        step (double step)
        major-step (double major-step)
        y (double (or y 0.0))
        coords (range (- size) (+ size 1.0e-6) step)
        data (transient [])
        major? (fn [v]
                 (let [r (Math/abs (rem v major-step))]
                   (< r (* 0.5 step))))]
    (doseq [z coords]
      (let [[r g b] (if (major? z) major-color minor-color)]
        (conj! data (float (- size)))
        (conj! data (float y))
        (conj! data (float z))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))
        (conj! data (float size))
        (conj! data (float y))
        (conj! data (float z))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))))
    (doseq [x coords]
      (let [[r g b] (if (major? x) major-color minor-color)]
        (conj! data (float x))
        (conj! data (float y))
        (conj! data (float (- size)))
        (conj! data (float r))
        (conj! data (float g))
        (conj! data (float b))
        (conj! data (float x))
        (conj! data (float y))
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

(defn- build-ui-rect-vao
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        stride (* 2 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER (* 6 2 Float/BYTES) GL15/GL_STREAM_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn- update-ui-rect-buffer!
  [^java.nio.FloatBuffer buf x y w h]
  (.clear buf)
  (.put buf (float-array
             [x y
              (+ x w) y
              (+ x w) (+ y h)
              (+ x w) (+ y h)
              x (+ y h)
              x y]))
  (.flip buf)
  buf)

(defn- update-ui-text-quad!
  [win-w win-h]
  (let [{:keys [ui-text-vbo ui-text-vertex-buffer ui-text-tex-w ui-text-tex-h]} @state
        tex-w (max 1 ui-text-tex-w)
        tex-h (max 1 ui-text-tex-h)
        u-right (float (/ win-w (double tex-w)))
        v-bottom (float (/ win-h (double tex-h)))]
    (.clear ui-text-vertex-buffer)
    (.put ui-text-vertex-buffer (float-array
                                 [0.0 0.0 0.0 0.0
                                  (float win-w) 0.0 u-right 0.0
                                  (float win-w) (float win-h) u-right v-bottom
                                  (float win-w) (float win-h) u-right v-bottom
                                  0.0 (float win-h) 0.0 v-bottom
                                  0.0 0.0 0.0 0.0]))
    (.flip ui-text-vertex-buffer)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER ui-text-vbo)
    (GL15/glBufferSubData GL15/GL_ARRAY_BUFFER 0 ui-text-vertex-buffer)))

(defn- ensure-ui-texture!
  [win-w win-h]
  (when (and (pos? win-w) (pos? win-h))
    (let [{:keys [ui-text-tex ui-text-tex-w ui-text-tex-h]} @state
          need-resize? (or (zero? ui-text-tex)
                           (< ui-text-tex-w win-w)
                           (< ui-text-tex-h win-h))
          tex-w (max ui-text-tex-w win-w)
          tex-h (max ui-text-tex-h win-h)]
      (when need-resize?
        (when (pos? ui-text-tex)
          (GL11/glDeleteTextures ui-text-tex))
        (let [tex (core/create-overlay-texture tex-w tex-h)
              bytes (byte-array (* tex-w tex-h))
              buffer (BufferUtils/createByteBuffer (* tex-w tex-h))]
          (swap! state assoc
                 :ui-text-tex tex
                 :ui-text-tex-w tex-w
                 :ui-text-tex-h tex-h
                 :ui-text-bytes bytes
                 :ui-text-buffer buffer)
          (update-ui-text-quad! win-w win-h)))
      (when (and (pos? ui-text-tex) (not need-resize?))
        (update-ui-text-quad! win-w win-h)))))

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
  [len shaft-radius]
  ;; 使用长方体代替线段，避免 macOS 核心模式线宽被限制为 1.0
  (let [r (double (max 0.001 (or shaft-radius 0.02)))
        red [1.0 0.0 0.0]
        green [0.0 1.0 0.0]
        blue [0.0 0.0 1.0]
        data (transient [])
        push-vertex! (fn [p color]
                       (conj! data (float (nth p 0)))
                       (conj! data (float (nth p 1)))
                       (conj! data (float (nth p 2)))
                       (conj! data (float (nth color 0)))
                       (conj! data (float (nth color 1)))
                       (conj! data (float (nth color 2))))
        push-tri! (fn [a b c color]
                    (push-vertex! a color)
                    (push-vertex! b color)
                    (push-vertex! c color))
        box! (fn [xmin xmax ymin ymax zmin zmax color]
               (let [p000 [xmin ymin zmin]
                     p001 [xmin ymin zmax]
                     p010 [xmin ymax zmin]
                     p011 [xmin ymax zmax]
                     p100 [xmax ymin zmin]
                     p101 [xmax ymin zmax]
                     p110 [xmax ymax zmin]
                     p111 [xmax ymax zmax]]
                 ;; +X / -X
                 (push-tri! p100 p110 p111 color)
                 (push-tri! p100 p111 p101 color)
                 (push-tri! p000 p011 p010 color)
                 (push-tri! p000 p001 p011 color)
                 ;; +Y / -Y
                 (push-tri! p010 p110 p111 color)
                 (push-tri! p010 p111 p011 color)
                 (push-tri! p000 p101 p100 color)
                 (push-tri! p000 p001 p101 color)
                 ;; +Z / -Z
                 (push-tri! p001 p011 p111 color)
                 (push-tri! p001 p111 p101 color)
                 (push-tri! p000 p110 p010 color)
                 (push-tri! p000 p100 p110 color)))]
    ;; X 轴(红色)
    (box! 0.0 (double len) (- r) r (- r) r red)
    ;; Y 轴(绿色)
    (box! (- r) r 0.0 (double len) (- r) r green)
    ;; Z 轴(蓝色)
    (box! (- r) r (- r) r 0.0 (double len) blue)
    (float-array (persistent! data))))

(defn- axis-arrow-vertices
  [len arrow-len arrow-r]
  ;; 使用圆锥箭头，确保轴向方向清晰
  (let [segments 16
        two-pi (* 2.0 Math/PI)
        red [1.0 0.0 0.0]
        green [0.0 1.0 0.0]
        blue [0.0 0.0 1.0]
        data (transient [])
        push-vertex! (fn [p color]
                       (conj! data (float (nth p 0)))
                       (conj! data (float (nth p 1)))
                       (conj! data (float (nth p 2)))
                       (conj! data (float (nth color 0)))
                       (conj! data (float (nth color 1)))
                       (conj! data (float (nth color 2))))
        push-tri! (fn [a b c color]
                    (push-vertex! a color)
                    (push-vertex! b color)
                    (push-vertex! c color))
        cone-x! (fn [color]
                  (let [tip [len 0.0 0.0]
                        base-x (- len arrow-len)]
                    (doseq [i (range segments)]
                      (let [a (* two-pi (/ (double i) segments))
                            b (* two-pi (/ (double (inc i)) segments))
                            p1 [base-x (* arrow-r (Math/cos a)) (* arrow-r (Math/sin a))]
                            p2 [base-x (* arrow-r (Math/cos b)) (* arrow-r (Math/sin b))]]
                        (push-tri! tip p1 p2 color)))))
        cone-y! (fn [color]
                  (let [tip [0.0 len 0.0]
                        base-y (- len arrow-len)]
                    (doseq [i (range segments)]
                      (let [a (* two-pi (/ (double i) segments))
                            b (* two-pi (/ (double (inc i)) segments))
                            p1 [(* arrow-r (Math/cos a)) base-y (* arrow-r (Math/sin a))]
                            p2 [(* arrow-r (Math/cos b)) base-y (* arrow-r (Math/sin b))]]
                        (push-tri! tip p1 p2 color)))))
        cone-z! (fn [color]
                  (let [tip [0.0 0.0 len]
                        base-z (- len arrow-len)]
                    (doseq [i (range segments)]
                      (let [a (* two-pi (/ (double i) segments))
                            b (* two-pi (/ (double (inc i)) segments))
                            p1 [(* arrow-r (Math/cos a)) (* arrow-r (Math/sin a)) base-z]
                            p2 [(* arrow-r (Math/cos b)) (* arrow-r (Math/sin b)) base-z]]
                        (push-tri! tip p1 p2 color)))))]
    (cone-x! red)
    (cone-y! green)
    (cone-z! blue)
    (float-array (persistent! data))))

(defn- camera-position
  [{:keys [yaw pitch distance]}]
  ;; 轨道相机位置，用于高光与头灯计算
  (let [yaw (double (or yaw 0.0))
        pitch (double (or pitch 0.0))
        dist (double (or distance 3.0))
        cy (Math/cos yaw)
        sy (Math/sin yaw)
        cp (Math/cos pitch)
        sp (Math/sin pitch)]
    [(* dist sy cp)
     (* dist sp)
     (* dist cy cp)]))

(defn- normalize-light
  [light camera-pos]
  (let [enabled? (if (contains? light :enabled?) (:enabled? light) true)]
    (when enabled?
      (let [pos (if (:follow-camera? light) camera-pos (or (:pos light) [0.0 0.0 0.0]))
            color (or (:color light) [1.0 1.0 1.0])
            intensity (double (or (:intensity light) 1.0))]
        {:pos pos :color color :intensity intensity}))))

(defn- active-lights
  [camera-pos]
  (->> (:lights @lighting-style)
       (map #(normalize-light % camera-pos))
       (remove nil?)
       (take 4)
       (vec)))

(defn- set-uniform-vec3-array!
  [program base-name values]
  ;; 逐元素设置数组 uniform，避免假设连续 location
  (doseq [[idx [x y z]] (map-indexed vector values)]
    (let [loc (GL20/glGetUniformLocation program (str base-name "[" idx "]"))]
      (when (<= 0 loc)
        (GL20/glUniform3f loc (float x) (float y) (float z))))))

(defn- set-uniform-float-array!
  [program base-name values]
  ;; 逐元素设置数组 uniform，避免假设连续 location
  (doseq [[idx v] (map-indexed vector values)]
    (let [loc (GL20/glGetUniformLocation program (str base-name "[" idx "]"))]
      (when (<= 0 loc)
        (GL20/glUniform1f loc (float v))))))

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
        ui-rect-program (core/create-program ui-rect-vs-source ui-rect-fs-source)
        ui-text-program (core/create-program (core/slurp-resource "shaders/overlay.vert")
                                             (core/slurp-resource "shaders/overlay.frag"))
        ui-rect-viewport-loc (GL20/glGetUniformLocation ui-rect-program "uViewport")
        ui-rect-color-loc (GL20/glGetUniformLocation ui-rect-program "uColor")
        ui-text-viewport-loc (GL20/glGetUniformLocation ui-text-program "uViewport")
        ui-text-color-loc (GL20/glGetUniformLocation ui-text-program "uTextColor")
        ui-text-sampler-loc (GL20/glGetUniformLocation ui-text-program "uText")
        {:keys [length arrow-length arrow-radius shaft-radius]} @axis-style
        {line-vao :vao line-vbo :vbo line-count :count}
        (build-axis-vao (axis-line-vertices length shaft-radius))
        {arrow-vao :vao arrow-vbo :vbo arrow-count :count}
        (build-axis-vao (axis-arrow-vertices length arrow-length arrow-radius))
        {grid-vao :vao grid-vbo :vbo grid-count :count} (build-grid-vao)
        ;; Cube resources
        cube-program (core/create-program cube-vs-source cube-fs-source)
        {point-vao :vao point-vbo :vbo} (build-point-vao)
        {sprite-vao :vao sprite-vbo :vbo} (build-sprite-vao)
        {overlay-vao :vao overlay-vbo :vbo} (build-overlay-vao)
        {ui-rect-vao :vao ui-rect-vbo :vbo} (build-ui-rect-vao)
        {ui-text-vao :vao ui-text-vbo :vbo} (core/create-overlay-quad)
        ui-rect-buffer (BufferUtils/createFloatBuffer (* 6 2))
        ui-text-vertex-buffer (BufferUtils/createFloatBuffer (* 6 4))
        {mesh-vao :vao mesh-vbo :vbo mesh-ebo :ebo} (build-mesh-vao)
        {spring-vao :vao spring-vbo :vbo} (build-spring-vao)
        {cube-vao :vao cube-vbo :vbo cube-count :count} (create-cube-vao)]
    (GL20/glUseProgram program)
    (when (<= 0 tex-loc)
      (GL20/glUniform1i tex-loc 0))
    (GL20/glUseProgram ui-text-program)
    (when (<= 0 ui-text-sampler-loc)
      (GL20/glUniform1i ui-text-sampler-loc 0))
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
           :overlay-vbo overlay-vbo
           :ui-rect-program ui-rect-program
           :ui-rect-vao ui-rect-vao
           :ui-rect-vbo ui-rect-vbo
           :ui-rect-buffer ui-rect-buffer
           :ui-rect-viewport-loc ui-rect-viewport-loc
           :ui-rect-color-loc ui-rect-color-loc
           :ui-text-program ui-text-program
           :ui-text-vao ui-text-vao
           :ui-text-vbo ui-text-vbo
           :ui-text-vertex-buffer ui-text-vertex-buffer
           :ui-text-viewport-loc ui-text-viewport-loc
           :ui-text-color-loc ui-text-color-loc)
    (let [win-w (or (get-in @app [:input :win-width]) 0)
          win-h (or (get-in @app [:input :win-height]) 0)]
      (ensure-ui-texture! win-w win-h))
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
        {:keys [length arrow-length arrow-radius shaft-radius]} @axis-style
        {line-vao :vao line-vbo :vbo line-count :count}
        (build-axis-vao (axis-line-vertices length shaft-radius))
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
                ui-rect-program ui-rect-vao ui-rect-vbo
                ui-text-program ui-text-vao ui-text-vbo ui-text-tex
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
    (delete-if-positive overlay-vao #(GL30/glDeleteVertexArrays %))
    ;; 清理 UI 资源
    (delete-if-positive ui-rect-program #(GL20/glDeleteProgram %))
    (delete-if-positive ui-rect-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive ui-rect-vao #(GL30/glDeleteVertexArrays %))
    (delete-if-positive ui-text-program #(GL20/glDeleteProgram %))
    (delete-if-positive ui-text-vbo #(GL15/glDeleteBuffers %))
    (delete-if-positive ui-text-vao #(GL30/glDeleteVertexArrays %))
    (delete-if-positive ui-text-tex #(GL11/glDeleteTextures %)))
  (reset! state {:program 0 :vao 0 :vbo 0 :ebo 0 :tex 0 :index-count 0
                 :axis-program 0
                 :axis-line-vao 0 :axis-line-vbo 0 :axis-line-count 0
                 :axis-arrow-vao 0 :axis-arrow-vbo 0 :axis-arrow-count 0
                 :cube-program 0 :cube-vao 0 :cube-vbo 0 :cube-vertex-count 0
                 :point-program 0 :point-vao 0 :point-vbo 0 :point-count 0
                 :sprite-program 0 :sprite-vao 0 :sprite-vbo 0 :sprite-count 0
                 :mesh-vao 0 :mesh-vbo 0 :mesh-ebo 0 :mesh-index-count 0
                 :spring-vao 0 :spring-vbo 0 :spring-count 0
                 :overlay-program 0 :overlay-vao 0 :overlay-vbo 0
                 :ui-rect-program 0 :ui-rect-vao 0 :ui-rect-vbo 0
                 :ui-rect-buffer nil :ui-rect-viewport-loc -1 :ui-rect-color-loc -1
                 :ui-text-program 0 :ui-text-vao 0 :ui-text-vbo 0
                 :ui-text-tex 0 :ui-text-tex-w 0 :ui-text-tex-h 0
                 :ui-text-bytes nil :ui-text-buffer nil :ui-text-vertex-buffer nil
                 :ui-text-viewport-loc -1 :ui-text-color-loc -1}))

(defn- render-mode->gl
  "将渲染模式映射为 OpenGL 多边形模式。"
  [mode]
  (case mode
    :wire GL11/GL_LINE
    :point GL11/GL_POINT
    GL11/GL_FILL))

(defn- with-polygon-mode
  "在指定多边形模式下执行绘制。"
  [mode f]
  (let [gl-mode (render-mode->gl mode)]
    (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK gl-mode)
    (try
      (f)
      (finally
        (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK GL11/GL_FILL)))))

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
    (if (:backface-cull? @display-state)
      (do
        (GL11/glEnable GL11/GL_CULL_FACE)
        (GL11/glCullFace GL11/GL_BACK))
      (GL11/glDisable GL11/GL_CULL_FACE))
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
          camera-pos (camera-position {:yaw yaw :pitch pitch :distance distance})
          mat-buf (BufferUtils/createFloatBuffer 16)
          axis-mvp-loc (GL20/glGetUniformLocation axis-program "mvp")
          ;; Cube shader locations
          cube-proj-loc (GL20/glGetUniformLocation cube-program "projection")
          cube-view-loc (GL20/glGetUniformLocation cube-program "view")
          cube-model-loc (GL20/glGetUniformLocation cube-program "model")
          cube-color-loc (GL20/glGetUniformLocation cube-program "uColor")
          cube-viewpos-loc (GL20/glGetUniformLocation cube-program "uViewPos")
          cube-ambient-loc (GL20/glGetUniformLocation cube-program "uAmbient")
          cube-specular-loc (GL20/glGetUniformLocation cube-program "uSpecularStrength")
          cube-shininess-loc (GL20/glGetUniformLocation cube-program "uShininess")
          cube-light-count-loc (GL20/glGetUniformLocation cube-program "uLightCount")
          sprite-mvp-loc (GL20/glGetUniformLocation sprite-program "mvp")
          sprite-soft-loc (GL20/glGetUniformLocation sprite-program "uSoftness")]

      ;; Draw grid
      (when (:show-grid? @display-state)
        (let [mvp (doto (Matrix4f. projection) (.mul view))
              grid-width (:line-width @grid-style)]
          (when (pos? grid-count)
            (GL20/glUseProgram axis-program)
            (upload-mat! mvp mat-buf axis-mvp-loc)
            (GL11/glLineWidth (float grid-width))
            (GL30/glBindVertexArray grid-vao)
            (GL11/glDrawArrays GL11/GL_LINES 0 grid-count)
            (GL11/glLineWidth 1.0))))

      ;; Draw axes
      (when (:show-axes? @display-state)
        (let [mvp (doto (Matrix4f. projection) (.mul view))]
          (GL20/glUseProgram axis-program)
          (upload-mat! mvp mat-buf axis-mvp-loc)
          (GL30/glBindVertexArray axis-line-vao)
          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 axis-line-count)
          (GL30/glBindVertexArray axis-arrow-vao)
          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 axis-arrow-count)
          (GL11/glLineWidth 1.0)))

      ;; Draw spring lines
      (when (and (:show-lines? @display-state) (pos? spring-count))
        (let [mvp (doto (Matrix4f. projection) (.mul view))]
          (GL20/glUseProgram axis-program)
          (upload-mat! mvp mat-buf axis-mvp-loc)
          (GL11/glLineWidth 1.5)
          (GL30/glBindVertexArray spring-vao)
          (GL11/glDrawArrays GL11/GL_LINES 0 spring-count)
          (GL11/glLineWidth 1.0)))

      ;; Draw point cloud
      (when (and (:show-points? @display-state) (pos? point-count))
        (let [mvp (doto (Matrix4f. projection) (.mul view))
              point-mvp-loc (GL20/glGetUniformLocation point-program "mvp")]
          (GL20/glUseProgram point-program)
          (upload-mat! mvp mat-buf point-mvp-loc)
          (GL30/glBindVertexArray point-vao)
          (GL11/glDrawArrays GL11/GL_POINTS 0 point-count)))

      ;; Draw sprites (透明排序后绘制)
      (when (:show-points? @display-state)
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
              (GL11/glDisable GL11/GL_BLEND)))))

      (letfn [(draw-geometry! [mode]
                (with-polygon-mode mode
                  (fn []
                    ;; Draw cubes
                    (when (:show-cubes? @display-state)
                      (GL20/glUseProgram cube-program)
                      (when (<= 0 cube-proj-loc)
                        (upload-mat! projection mat-buf cube-proj-loc))
                      (when (<= 0 cube-view-loc)
                        (upload-mat! view mat-buf cube-view-loc))
                      (let [{:keys [ambient specular-strength shininess]} @lighting-style
                            lighting? (:lighting? @display-state)
                            ambient (if lighting? ambient [1.0 1.0 1.0])
                            specular-strength (if lighting? specular-strength 0.0)
                            shininess (if lighting? shininess 1.0)
                            lights (if lighting? (active-lights camera-pos) [])]
                        (when (<= 0 cube-viewpos-loc)
                          (GL20/glUniform3f cube-viewpos-loc
                                            (float (nth camera-pos 0))
                                            (float (nth camera-pos 1))
                                            (float (nth camera-pos 2))))
                        (when (and (<= 0 cube-ambient-loc) ambient)
                          (GL20/glUniform3f cube-ambient-loc
                                            (float (nth ambient 0))
                                            (float (nth ambient 1))
                                            (float (nth ambient 2))))
                        (when (<= 0 cube-specular-loc)
                          (GL20/glUniform1f cube-specular-loc (float (or specular-strength 0.0))))
                        (when (<= 0 cube-shininess-loc)
                          (GL20/glUniform1f cube-shininess-loc (float (or shininess 1.0))))
                        (when (<= 0 cube-light-count-loc)
                          (GL20/glUniform1i cube-light-count-loc (count lights)))
                        (set-uniform-vec3-array! cube-program "uLightPos" (map :pos lights))
                        (set-uniform-vec3-array! cube-program "uLightColor" (map :color lights))
                        (set-uniform-float-array! cube-program "uLightIntensity" (map :intensity lights)))

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
                          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 cube-vertex-count))))

                    ;; Draw generic mesh (e.g. sphere)
                    (when (and (:show-mesh? @display-state) (pos? mesh-index-count))
                      (let [{:keys [pos rot scale color]} @mesh-style
                            model (compose-transform pos rot scale)]
                        (when (<= 0 cube-model-loc)
                          (upload-mat! model mat-buf cube-model-loc))
                        (when (<= 0 cube-color-loc)
                          (GL20/glUniform3f cube-color-loc (float (nth color 0)) (float (nth color 1)) (float (nth color 2))))
                        (GL30/glBindVertexArray mesh-vao)
                        (GL11/glDrawElements GL11/GL_TRIANGLES mesh-index-count GL11/GL_UNSIGNED_INT 0)
                        (when (and (= mode :fill) (:enabled? @wireframe-overlay))
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
                            (GL11/glLineWidth 1.0))))))))]
        (when (:display-filled? @display-state)
          (draw-geometry! :fill))
        (when (:display-wireframe? @display-state)
          (draw-geometry! :wire))
        (when (:display-points? @display-state)
          (draw-geometry! :point)))

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
  "更新坐标轴样式. 支持的键: :length, :arrow-length, :arrow-radius, :shaft-radius.
  说明: 轴身使用几何体渲染, 建议用 :shaft-radius 控制粗细.
  几何变化会在 GL 线程上应用. "
  [style]
  (let [keys-to-update (select-keys style [:length :arrow-length :arrow-radius :shaft-radius])
        geometry? (some #(contains? keys-to-update %) [:length :arrow-length :arrow-radius :shaft-radius])]
    (swap! axis-style merge keys-to-update)
    (if geometry?
      (enqueue! rebuild-axes!)
      (let [reply (async/promise-chan)]
        (async/put! reply {:ok :no-op})
        reply))))

(defn set-grid-style!
  "更新网格样式. 支持的键: :size, :step, :major-step, :y, :minor-color, :major-color, :line-width.
  几何变化会在 GL 线程上应用. "
  [style]
  (let [keys-to-update (select-keys style [:size :step :major-step :y :minor-color :major-color :line-width])
        geometry? (some #(contains? keys-to-update %) [:size :step :major-step :y :minor-color :major-color])]
    (swap! grid-style merge keys-to-update)
    (if geometry?
      (enqueue! rebuild-grid!)
      (let [reply (async/promise-chan)]
        (async/put! reply {:ok :no-op})
        reply))))

(defn lighting
  "获取当前光源配置."
  []
  @lighting-style)

(defn set-lighting!
  "更新光源配置. 支持的键: :ambient, :specular-strength, :shininess, :lights."
  [style]
  (let [keys-to-update (select-keys style [:ambient :specular-strength :shininess :lights])]
    (swap! lighting-style merge keys-to-update))
  :ok)

(defn set-light!
  "更新单个光源(按 :id). 参数:
    id - 光源标识关键字
    patch - 需要合并的字段."
  [id patch]
  (swap! lighting-style update :lights
         (fn [lights]
           (mapv (fn [light]
                   (if (= (:id light) id)
                     (merge light patch)
                     light))
                 lights)))
  :ok)

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

(defn- demo-command-specs
  "收集默认演示命令的键位映射。"
  []
  (let [specs (atom [])]
    (kinput/register-demo-commands!
     (fn [key action cmd]
       (swap! specs conj {:key key :action action :cmd cmd})))
    @specs))

(defn- command-label
  "生成命令在菜单中的显示标题。"
  [cmd]
  (case cmd
    :demo "Demo"
    :pause "Pause / Resume"
    (kui/format-title cmd)))

(defn- command-action
  "返回命令对应的执行函数。"
  [cmd]
  (case cmd
    :demo (fn [] (start-demo!))
    :pause (fn []
             (if (get-in @app [:flags :paused?])
               (resume!)
               (pause!)))
    (fn [] (set-scene! cmd))))

(defn- reset-camera!
  "重置相机视角。"
  []
  (swap! app assoc :camera {:yaw 0.0 :pitch 0.0 :distance 3.0})
  :ok)

(defn- clear-objects!
  "清空当前场景中可见对象。"
  []
  (remove-all-cubes!)
  (clear-mesh!)
  (clear-point-cloud!)
  (clear-line-segments!)
  (clear-sprites!)
  (clear-rig!)
  :ok)

(defn- line-data-from-points
  "将折线点转换为线段数据。"
  [points color]
  (let [[r g b] (map float (take 3 (or color [0.9 0.6 0.2])))]
    (vec
     (mapcat (fn [[a b]]
               (let [[ax ay az] a
                     [bx by bz] b]
                 [(float ax) (float ay) (float az)
                  r g b
                  (float bx) (float by) (float bz)
                  r g b]))
             (partition 2 1 points)))))

(defn- set-curve!
  "设置曲线线段。"
  [points]
  (let [line-data (line-data-from-points points [0.9 0.6 0.2])]
    (swap! display-state assoc :show-lines? true)
    (set-line-segments! line-data))
  :ok)

(defn- set-mesh-from!
  "设置网格并更新显示样式。"
  [mesh]
  (let [{:keys [vertices indices]} mesh
        color [0.25 0.65 0.9]]
    (set-mesh! vertices indices)
    (set-mesh-style! {:pos [0.0 0.0 0.0]
                      :rot [0.0 0.0 0.0]
                      :scale [1.0 1.0 1.0]
                      :color color}))
  :ok)

(defn- toggle-display!
  "切换显示状态。"
  [k]
  (swap! display-state update k not)
  :ok)

(defn- set-render-mode!
  "设置多边形显示模式。"
  [mode]
  (swap! display-state assoc
         :display-filled? (= mode :fill)
         :display-wireframe? (= mode :wire)
         :display-points? (= mode :point))
  :ok)

(defn- toggle-smooth-shading!
  "切换平滑着色并刷新网格法线。"
  []
  (swap! display-state update :smooth-shading? not)
  (apply-mesh-shading!)
  :ok)

(defn- set-objects-visible!
  "批量设置对象显示状态。"
  [visible?]
  (swap! display-state merge
         {:show-cubes? visible?
          :show-mesh? visible?
          :show-points? visible?
          :show-lines? visible?})
  :ok)

(defn- show-all!
  "显示所有对象。"
  []
  (set-objects-visible! true))

(defn- hide-all!
  "隐藏所有对象。"
  []
  (set-objects-visible! false))

(defn- set-theme-bright!
  "切换为明亮主题。"
  []
  (set-clear-color! 0.94 0.95 0.97 1.0)
  (set-grid-style! {:minor-color [0.18 0.18 0.24]
                    :major-color [0.28 0.28 0.34]})
  (set-lighting! {:ambient [0.14 0.14 0.16]
                  :specular-strength 0.35
                  :shininess 32.0})
  :ok)

(defn- set-theme-dark!
  "切换为暗色主题。"
  []
  (set-clear-color! 0.05 0.05 0.08 1.0)
  (set-grid-style! {:minor-color [0.1 0.1 0.14]
                    :major-color [0.2 0.2 0.26]})
  (set-lighting! {:ambient [0.1 0.1 0.12]
                  :specular-strength 0.25
                  :shininess 24.0})
  :ok)

(defn- info-lines
  "生成信息面板内容。"
  []
  [(str "Scene: " (current-scene))
   (str "Paused: " (get-in @app [:flags :paused?]))
   (str "Manual: " (get-in @app [:flags :manual-play?]))
   (str "Cubes: " (cube-count))
   (str "Mesh: " (if (pos? (:mesh-index-count @state)) "On" "Off"))
   (str "Points: " (count (:points @point-cloud-state)))
   (str "Lines: " (:spring-count @state))])

(defn- shapes-lines
  "生成形状概览。"
  []
  [(str "Cubes: " (cube-count))
   (str "Mesh: " (if (pos? (:mesh-index-count @state)) "On" "Off"))
   (str "Points: " (count (:points @point-cloud-state)))
   (str "Lines: " (:spring-count @state))])

(defn- motions-lines
  "生成时间线信息。"
  []
  (let [{:keys [enabled? items index elapsed]} (:timeline @app)]
    [(str "Timeline: " (if enabled? "On" "Off"))
     (str "Items: " (count items))
     (str "Index: " index)
     (str "Elapsed: " (format "%.2f" (double elapsed)))]))

(defn- show-info!
  "显示信息面板。"
  [title lines]
  (kui/set-info! title lines))

(defn- toggle-info!
  "切换信息面板。"
  []
  (kui/toggle-info!))

(defn- set-interactive-mode!
  "设置交互模式。进入后隐藏 UI，按 ESC 退出。"
  [enabled?]
  (swap! app assoc-in [:flags :ui-interactive?] (boolean enabled?))
  (if enabled?
    (do
      (kui/hide-menu!)
      (kui/clear-info!))
    (kui/show-menu!))
  :ok)

(defn- interactive-mode?
  "判断是否处于交互模式。"
  []
  (get-in @app [:flags :ui-interactive?]))

(defn- show-inspector!
  "显示综合信息。"
  []
  (show-info! "Inspector" (info-lines)))

(defn- show-shapes!
  "显示形状信息。"
  []
  (show-info! "Shapes" (shapes-lines)))

(defn- show-motions!
  "显示时间线信息。"
  []
  (show-info! "Motions" (motions-lines)))

(defn- request-exit!
  "请求关闭窗口。"
  []
  (when-let [win (:window @app)]
    (GLFW/glfwSetWindowShouldClose win true))
  :ok)

(defn- show-unimplemented!
  "显示未实现提示。"
  [title]
  (show-info! title ["该功能尚未实现。"])
  :ok)

(defn- new-scene!
  "创建新场景并清空当前对象。"
  []
  (stop-demo!)
  (set-scene! :baseline)
  (clear-objects!)
  :ok)

(defn- frame-selection!
  "当前版本没有选择集，先重置相机。"
  []
  (reset-camera!))

(defn- init-ui!
  "初始化菜单与按钮。"
  []
  (let [scene-table (kui/make-command-table
                     "Scene"
                     [(kui/command-entry GLFW/GLFW_KEY_N "New Scene" (fn [] (new-scene!)))
                      (kui/command-entry GLFW/GLFW_KEY_E "Export OBJ File"
                                         (fn [] (show-unimplemented! "Export OBJ File")))
                      (kui/command-entry GLFW/GLFW_KEY_U "Export USD File"
                                         (fn [] (show-unimplemented! "Export USD File")))
                      (kui/command-entry GLFW/GLFW_KEY_I "Initialize Scene" (fn [] (reset-scene!)))
                      (kui/command-entry GLFW/GLFW_KEY_Q "Quit Scene" (fn [] (request-exit!)))])

        edit-table (kui/make-command-table
                    "Edit"
                    [(kui/command-entry GLFW/GLFW_KEY_BACKSPACE "Delete" (fn [] (clear-objects!)))
                     (kui/command-entry GLFW/GLFW_KEY_S "Show" (fn [] (show-all!)))
                     (kui/command-entry GLFW/GLFW_KEY_H "Hide" (fn [] (hide-all!)))])

        curve-table (kui/make-command-table
                     "Create Curve"
                     [(kui/command-entry GLFW/GLFW_KEY_L "Line Curve"
                                         (fn [] (set-curve! (kgeom/line-points [0.0 0.0 0.0]
                                                                               [2.0 0.0 0.0]
                                                                               16))))
                      (kui/command-entry GLFW/GLFW_KEY_R "Rectangle Curve"
                                         (fn [] (set-curve! (kgeom/rectangle-points 2.0 1.0 32))))
                      (kui/command-entry GLFW/GLFW_KEY_S "Square Curve"
                                         (fn [] (set-curve! (kgeom/square-points 1.5 32))))
                      (kui/command-entry GLFW/GLFW_KEY_C "Circle Curve"
                                         (fn [] (set-curve! (kgeom/circle-points 2.0 48))))
                      (kui/command-entry GLFW/GLFW_KEY_A "Arc Curve"
                                         (fn [] (set-curve! (kgeom/arc-points 2.0 0.0 90.0 24))))
                      (kui/command-entry GLFW/GLFW_KEY_N "Sine Curve"
                                         (fn [] (set-curve! (kgeom/sine-curve-points 360.0 1.0 2.0 1.0 64))))
                      (kui/command-entry GLFW/GLFW_KEY_P "Spiral Curve"
                                         (fn [] (set-curve! (kgeom/spiral-points 0.4 2.0 -1.0 4 96))))])

        poly-table (kui/make-command-table
                    "Create Polyhedron"
                    [(kui/command-entry GLFW/GLFW_KEY_T "Tetrahedron"
                                        (fn [] (set-mesh-from! (kgeom/tetrahedron-mesh 2.0))))
                     (kui/command-entry GLFW/GLFW_KEY_C "Cube"
                                        (fn [] (set-mesh-from! (kgeom/cube-mesh 2.0))))
                     (kui/command-entry GLFW/GLFW_KEY_O "Octahedron"
                                        (fn [] (set-mesh-from! (kgeom/octahedron-mesh 2.0))))
                     (kui/command-entry GLFW/GLFW_KEY_D "Dodecahedron"
                                        (fn [] (set-mesh-from! (kgeom/dodecahedron-mesh 2.0))))
                     (kui/command-entry GLFW/GLFW_KEY_I "Icosahedron"
                                        (fn [] (set-mesh-from! (kgeom/icosahedron-mesh 2.0))))
                     (kui/command-entry GLFW/GLFW_KEY_S "Sphere"
                                        (fn [] (set-mesh-from! (kgeom/uv-sphere 24 24))))])

        create-table (kui/make-command-table
                      "Create"
                      [(kui/subtable-entry GLFW/GLFW_KEY_C "Create Curve" curve-table)
                       (kui/subtable-entry GLFW/GLFW_KEY_P "Create Polyhedron" poly-table)])

        inspect-table (kui/make-command-table
                       "Inspect"
                       [(kui/command-entry GLFW/GLFW_KEY_R "Reset Camera" (fn [] (reset-camera!)))
                        (kui/command-entry GLFW/GLFW_KEY_F "Frame Selection" (fn [] (frame-selection!)))
                        (kui/command-entry GLFW/GLFW_KEY_I "Inspector" (fn [] (show-inspector!)))
                        (kui/command-entry GLFW/GLFW_KEY_S "Shapes" (fn [] (show-shapes!)))
                        (kui/command-entry GLFW/GLFW_KEY_M "Motions" (fn [] (show-motions!)))])

        display-table (kui/make-command-table
                       "Display"
                       [(kui/command-entry GLFW/GLFW_KEY_GRAVE_ACCENT "Toggle Lighting"
                                           (fn [] (toggle-display! :lighting?)))
                        (kui/command-entry GLFW/GLFW_KEY_1 "Toggle Filled Display"
                                           (fn [] (toggle-display! :display-filled?)))
                        (kui/command-entry GLFW/GLFW_KEY_2 "Toggle Wireframe Display"
                                           (fn [] (toggle-display! :display-wireframe?)))
                        (kui/command-entry GLFW/GLFW_KEY_3 "Toggle Point Display"
                                           (fn [] (toggle-display! :display-points?)))
                        (kui/command-entry GLFW/GLFW_KEY_4 "Toggle Backface Culling"
                                           (fn [] (toggle-display! :backface-cull?)))
                        (kui/command-entry GLFW/GLFW_KEY_5 "Toggle Smooth Shading"
                                           (fn [] (toggle-smooth-shading!)))
                        (kui/command-entry GLFW/GLFW_KEY_6 "Toggle Ground Plane Display"
                                           (fn [] (toggle-display! :show-grid?)))
                        (kui/command-entry GLFW/GLFW_KEY_7 "Toggle World Axes Display"
                                           (fn [] (toggle-display! :show-axes?)))
                        (kui/command-entry GLFW/GLFW_KEY_8 "Set Bright Theme"
                                           (fn [] (set-theme-bright!)))
                        (kui/command-entry GLFW/GLFW_KEY_9 "Set Dark Theme"
                                           (fn [] (set-theme-dark!)))])

        context-table (kui/make-command-table
                       "Context"
                       [(kui/command-entry GLFW/GLFW_KEY_T "Transform Selection"
                                           (fn [] (show-unimplemented! "Transform Selection")))])

        root (kui/make-command-table
              "kons-9"
              [(kui/subtable-entry GLFW/GLFW_KEY_S "Scene" scene-table)
               (kui/subtable-entry GLFW/GLFW_KEY_E "Edit" edit-table)
               (kui/subtable-entry GLFW/GLFW_KEY_C "Create" create-table)
               (kui/subtable-entry GLFW/GLFW_KEY_I "Inspect" inspect-table)
               (kui/subtable-entry GLFW/GLFW_KEY_D "Display" display-table)
               (kui/command-entry GLFW/GLFW_KEY_M "Interactive Mode (ESC to exit)"
                                  (fn [] (set-interactive-mode! true)))
               (kui/subtable-entry GLFW/GLFW_KEY_X "Context" context-table)])

        buttons [{:id :menu
                  :label "Menu"
                  :action (fn [] (kui/toggle-menu!))}
                 {:id :demo
                  :label "Demo"
                  :action (fn [] (start-demo!))}
                 {:id :pause
                  :label (fn []
                           (if (get-in @app [:flags :paused?]) "Resume" "Pause"))
                  :action (fn []
                            (if (get-in @app [:flags :paused?])
                              (resume!)
                              (pause!)))}
                 {:id :info
                  :label "Info"
                  :action (fn [] (toggle-info!))}]]
    (kui/set-menu! root)
    (kui/set-buttons! buttons)
    :ok))

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

(defn- normalize3
  [x y z]
  (let [len (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (<= len 1.0e-9)
      [0.0 0.0 0.0]
      [(/ x len) (/ y len) (/ z len)])))

(defn- smooth-vertices
  "根据位置合并法线，生成平滑着色顶点数据。"
  [^floats vertices]
  (let [count (quot (alength vertices) 6)
        acc (transient {})
        out (float-array (alength vertices))]
    (dotimes [i count]
      (let [base (* i 6)
            x (aget vertices base)
            y (aget vertices (inc base))
            z (aget vertices (+ base 2))
            nx (aget vertices (+ base 3))
            ny (aget vertices (+ base 4))
            nz (aget vertices (+ base 5))
            key [x y z]
            [sx sy sz] (get acc key [0.0 0.0 0.0])]
        (assoc! acc key [(+ sx nx) (+ sy ny) (+ sz nz)])))
    (let [acc (persistent! acc)
          normals (reduce-kv (fn [m k [sx sy sz]]
                               (assoc m k (normalize3 sx sy sz)))
                             {} acc)]
      (dotimes [i count]
        (let [base (* i 6)
              x (aget vertices base)
              y (aget vertices (inc base))
              z (aget vertices (+ base 2))
              [nx ny nz] (get normals [x y z] [0.0 0.0 0.0])]
          (aset-float out base (float x))
          (aset-float out (inc base) (float y))
          (aset-float out (+ base 2) (float z))
          (aset-float out (+ base 3) (float nx))
          (aset-float out (+ base 4) (float ny))
          (aset-float out (+ base 5) (float nz)))))
    out))

(defn- upload-mesh!
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

(defn- apply-mesh-shading!
  []
  (when-let [{:keys [vertices indices]} (:base @mesh-data)]
    (let [smooth? (:smooth-shading? @display-state)
          smooth (when smooth? (or (get-in @mesh-data [:smooth :vertices])
                                   (smooth-vertices vertices)))
          vertices (if smooth? smooth vertices)]
      (when (and smooth? (nil? (get-in @mesh-data [:smooth :vertices])))
        (swap! mesh-data assoc :smooth {:vertices smooth :indices indices}))
      (upload-mesh! vertices indices))))

(defn set-mesh!
  "设置通用网格数据. vertices 为包含位置与法线的 float-array, indices 为 int-array. "
  [vertices indices]
  (swap! mesh-data assoc :base {:vertices vertices :indices indices}
         :smooth nil)
  (apply-mesh-shading!))

(defn clear-mesh!
  "清空通用网格. "
  []
  (swap! mesh-data assoc :base nil :smooth nil)
  (upload-mesh! (float-array []) (int-array [])))

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
           (cond
             (and (interactive-mode?) (= key GLFW/GLFW_KEY_ESCAPE) (= action GLFW/GLFW_PRESS))
             (set-interactive-mode! false)

             (interactive-mode?)
             (kinput/handle-key win key action)

             (kui/handle-key! key action)
             nil

             :else
             (kinput/handle-key win key action)))))
      (GLFW/glfwSetMouseButtonCallback
       window
       (reify GLFWMouseButtonCallbackI
         (invoke [_ win button action _]
           (when (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
             (let [xbuf (BufferUtils/createDoubleBuffer 1)
                   ybuf (BufferUtils/createDoubleBuffer 1)]
               (GLFW/glfwGetCursorPos win xbuf ybuf)
               (let [x (.get xbuf 0)
                     y (.get ybuf 0)
                     win-w (get-in @app [:input :win-width])
                     win-h (get-in @app [:input :win-height])
                     handled? (when-not (interactive-mode?)
                                (kui/handle-mouse-press! x y button action win-w win-h))]
                 (swap! app assoc-in [:input :last-x] x)
                 (swap! app assoc-in [:input :last-y] y)
                 (cond
                   handled? (swap! app assoc-in [:input :dragging?] false)
                   (= action GLFW/GLFW_PRESS) (swap! app assoc-in [:input :dragging?] true)
                   (= action GLFW/GLFW_RELEASE) (swap! app assoc-in [:input :dragging?] false))))))))
      (GLFW/glfwSetCursorPosCallback
       window
       (reify GLFWCursorPosCallbackI
         (invoke [_ _ xpos ypos]
           (let [{:keys [dragging? last-x last-y win-width win-height]} (:input @app)]
             (when-not (interactive-mode?)
               (kui/handle-mouse-move! xpos ypos win-width win-height))
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
      (init-ui!)
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
       (fn [_ key action]
         (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                    (= action GLFW/GLFW_PRESS)
                    (interactive-mode?))
           (set-interactive-mode! false))))
      (loop []
        (when-not (GLFW/glfwWindowShouldClose window)
          (drain-commands!)
          (update-time!)
          (let [dt (get-in @app [:time :dt])]
            (update-timeline! dt)
            (update-transition! dt)
            (update-scene! dt))
          (render-scene!)
          (render-ui!)
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
  (set-axis-style! {:shaft-radius 0.02
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
;; (async/<!! (hr/set-axis-style! {:shaft-radius 0.04
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
