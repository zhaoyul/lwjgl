(ns lwjgl.chapter1.camera
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWScrollCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
out vec2 TexCoord;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
    gl_Position = projection * view * model * vec4(aPos, 1.0);
    TexCoord = aTexCoord;
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform sampler2D texture1;
void main() {
    FragColor = texture(texture1, TexCoord);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- create-cube
  []
  (let [vertices (float-array
                  [;; positions        ;; tex coords
                   -0.5 -0.5 -0.5      0.0 0.0
                   0.5 -0.5 -0.5      1.0 0.0
                   0.5  0.5 -0.5      1.0 1.0
                   0.5  0.5 -0.5      1.0 1.0
                   -0.5  0.5 -0.5      0.0 1.0
                   -0.5 -0.5 -0.5      0.0 0.0

                   -0.5 -0.5  0.5      0.0 0.0
                   0.5 -0.5  0.5      1.0 0.0
                   0.5  0.5  0.5      1.0 1.0
                   0.5  0.5  0.5      1.0 1.0
                   -0.5  0.5  0.5      0.0 1.0
                   -0.5 -0.5  0.5      0.0 0.0

                   -0.5  0.5  0.5      1.0 0.0
                   -0.5  0.5 -0.5      1.0 1.0
                   -0.5 -0.5 -0.5      0.0 1.0
                   -0.5 -0.5 -0.5      0.0 1.0
                   -0.5 -0.5  0.5      0.0 0.0
                   -0.5  0.5  0.5      1.0 0.0

                   0.5  0.5  0.5      1.0 0.0
                   0.5  0.5 -0.5      1.0 1.0
                   0.5 -0.5 -0.5      0.0 1.0
                   0.5 -0.5 -0.5      0.0 1.0
                   0.5 -0.5  0.5      0.0 0.0
                   0.5  0.5  0.5      1.0 0.0

                   -0.5 -0.5 -0.5      0.0 1.0
                   0.5 -0.5 -0.5      1.0 1.0
                   0.5 -0.5  0.5      1.0 0.0
                   0.5 -0.5  0.5      1.0 0.0
                   -0.5 -0.5  0.5      0.0 0.0
                   -0.5 -0.5 -0.5      0.0 1.0

                   -0.5  0.5 -0.5      0.0 1.0
                   0.5  0.5 -0.5      1.0 1.0
                   0.5  0.5  0.5      1.0 0.0
                   0.5  0.5  0.5      1.0 0.0
                   -0.5  0.5  0.5      0.0 0.0
                   -0.5  0.5 -0.5      0.0 1.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 5 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn- create-texture
  [w h]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [checker (if (zero? (bit-and (+ (quot x 16) (quot y 16)) 1)) 240 110)
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (byte checker))
          (.put buf (inc idx) (byte checker))
          (.put buf (+ idx 2) (byte 200)))))
    (.flip buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.flip buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(def cube-positions
  [(Vector3f. 0.0 0.0 0.0)
   (Vector3f. 2.0 5.0 -15.0)
   (Vector3f. -1.5 -2.2 -2.5)
   (Vector3f. -3.8 -2.0 -12.3)
   (Vector3f. 2.4 -0.4 -3.5)
   (Vector3f. -1.7 3.0 -7.5)
   (Vector3f. 1.3 -2.0 -2.5)
   (Vector3f. 1.5 2.0 -2.5)
   (Vector3f. 1.5 0.2 -1.5)
   (Vector3f. -1.3 1.0 -1.5)])

(def ^:const base-rotation-step 0.25)
(def ^:const base-speed 2.5)
(def ^:const mouse-sensitivity 0.1)

(defn- make-camera-state
  []
  {:pos (Vector3f. 0.0 0.0 3.0)
   :front (Vector3f. 0.0 0.0 -1.0)
   :up (Vector3f. 0.0 1.0 0.0)
   :right (Vector3f. 1.0 0.0 0.0)
   :world-up (Vector3f. 0.0 1.0 0.0)
   :yaw (atom -90.0)
   :pitch (atom 0.0)
   :fov (atom 45.0)
   :first-mouse (atom true)
   :last-x (atom 400.0)
   :last-y (atom 300.0)})

(defn- update-camera-vectors!
  [{:keys [^Vector3f front ^Vector3f right ^Vector3f up ^Vector3f world-up yaw pitch]}]
  (let [yaw-rad (Math/toRadians ^double @yaw)
        pitch-rad (Math/toRadians ^double @pitch)
        fx (* (Math/cos yaw-rad) (Math/cos pitch-rad))
        fy (Math/sin pitch-rad)
        fz (* (Math/sin yaw-rad) (Math/cos pitch-rad))]
    (.set front (float fx) (float fy) (float fz))
    (.normalize front)
    (.cross front world-up right)
    (.normalize right)
    (.cross right front up)
    (.normalize up)))

(defn- process-keyboard!
  [window {:keys [^Vector3f pos ^Vector3f front ^Vector3f right] :as state} dt restrict-ground?]
  (let [velocity (float (* base-speed dt))]
    (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_W))
      (.fma pos front velocity))
    (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_S))
      (.fma pos front (- velocity)))
    (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_A))
      (.fma pos right (- velocity)))
    (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_D))
      (.fma pos right velocity))
    (when restrict-ground?
      (.setComponent pos 1 (float 0.0))))
  state)

(defn- handle-mouse!
  [state xpos ypos]
  (let [first? @(:first-mouse state)]
    (if first?
      (do
        (reset! (:last-x state) xpos)
        (reset! (:last-y state) ypos)
        (reset! (:first-mouse state) false))
      (let [xoffset (* mouse-sensitivity (- xpos @(:last-x state)))
            yoffset (* mouse-sensitivity (- @(:last-y state) ypos))
            new-yaw (+ @(:yaw state) xoffset)
            new-pitch (-> (+ @(:pitch state) yoffset)
                          (max -89.0)
                          (min 89.0))]
        (reset! (:last-x state) xpos)
        (reset! (:last-y state) ypos)
        (reset! (:yaw state) new-yaw)
        (reset! (:pitch state) new-pitch)
        (update-camera-vectors! state)))))

(defn- handle-scroll!
  [state yoffset]
  (let [clamped (-> (- @(:fov state) yoffset)
                    (max 1.0)
                    (min 45.0))]
    (reset! (:fov state) clamped)))

(defn- custom-look-at
  [^Vector3f position ^Vector3f target ^Vector3f up]
  (let [z (doto (Vector3f. position) (.sub target) (.normalize))
        x (doto (Vector3f. up) (.cross z) (.normalize))
        y (doto (Vector3f. z) (.cross x))]
    (doto (Matrix4f.)
      (.m00 (.x x)) (.m01 (.x y)) (.m02 (.x z)) (.m03 0.0)
      (.m10 (.y x)) (.m11 (.y y)) (.m12 (.y z)) (.m13 0.0)
      (.m20 (.z x)) (.m21 (.z y)) (.m22 (.z z)) (.m23 0.0)
      (.m30 (- (.dot x position))) (.m31 (- (.dot y position))) (.m32 (- (.dot z position))) (.m33 1.0))))

(defn- window-title
  [mode]
  (case mode
    :circle "LearnOpenGL - 7.1 Camera Circle (LWJGL)"
    :keyboard-dt "LearnOpenGL - 7.2 Camera Keyboard Delta Time (LWJGL)"
    :mouse-zoom "LearnOpenGL - 7.3 Camera Mouse + Zoom (LWJGL)"
    :class "LearnOpenGL - 7.4 Camera Class (LWJGL)"
    :exercise1 "LearnOpenGL - 7.5 Camera Exercise 1 (LWJGL)"
    :exercise2 "LearnOpenGL - 7.6 Camera Exercise 2 (LWJGL)"
    "LearnOpenGL - Camera (LWJGL)"))

(defn run-example!
  [mode]
  (let [width (atom 800)
        height (atom 600)
        error-callback (core/init-glfw!)
        window (core/create-window @width @height (window-title mode))]
    (try
      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo]} (create-cube)
            tex (create-texture 256 256)
            model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            tex-loc (GL20/glGetUniformLocation program "texture1")
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            view (Matrix4f.)
            projection (Matrix4f.)
            camera (make-camera-state)
            target (Vector3f.)
            last-time (atom (GLFW/glfwGetTime))]
        (try
          (core/init-viewport! window width height)
          (GLFW/glfwSetFramebufferSizeCallback
           window
           (reify GLFWFramebufferSizeCallbackI
             (invoke [_ _ w h]
               (reset! width w)
               (reset! height h)
               (GL11/glViewport 0 0 w h))))
          (GLFW/glfwSetKeyCallback
           window
           (reify GLFWKeyCallbackI
             (invoke [_ win key _ action _]
               (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                          (= action GLFW/GLFW_PRESS))
                 (GLFW/glfwSetWindowShouldClose win true)))))
          (when (contains? #{:mouse-zoom :class :exercise1 :exercise2} mode)
            (GLFW/glfwSetInputMode window GLFW/GLFW_CURSOR GLFW/GLFW_CURSOR_DISABLED)
            (GLFW/glfwSetCursorPosCallback
             window
             (reify GLFWCursorPosCallbackI
               (invoke [_ _ xpos ypos]
                 (handle-mouse! camera xpos ypos))))
            (GLFW/glfwSetScrollCallback
             window
             (reify GLFWScrollCallbackI
               (invoke [_ _ _ yoffset]
                 (handle-scroll! camera yoffset)))))
          (update-camera-vectors! camera)
          (GL20/glUseProgram program)
          (when (<= 0 tex-loc) (GL20/glUniform1i tex-loc 0))
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [current (GLFW/glfwGetTime)
                    dt (double (- current @last-time))]
                (reset! last-time current)
                (when (not= mode :circle)
                  (process-keyboard! window camera dt (= mode :exercise1)))
                (cond
                  (= mode :circle)
                  (let [radius 6.0
                        cam-x (* radius (Math/sin current))
                        cam-z (* radius (Math/cos current))]
                    (.identity view)
                    (.lookAt view (Vector3f. (float cam-x) 0.0 (float cam-z))
                             (Vector3f. 0.0 0.0 0.0)
                             (Vector3f. 0.0 1.0 0.0)))

                  (= mode :exercise2)
                  (let [^Vector3f pos (:pos camera)]
                    (.set target pos)
                    (.add target ^Vector3f (:front camera))
                    (.set view (custom-look-at pos target ^Vector3f (:up camera))))

                  :else
                  (let [^Vector3f pos (:pos camera)]
                    (.set target pos)
                    (.add target ^Vector3f (:front camera))
                    (.identity view)
                    (.lookAt view pos target ^Vector3f (:up camera))))
                (.identity projection)
                (.perspective projection
                              (float (Math/toRadians ^double @(:fov camera)))
                              (/ @width (float (max 1 @height)))
                              0.1 100.0)
                (GL11/glClearColor 0.08 0.1 0.12 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (upload-mat! view mat-buf view-loc)
                (upload-mat! projection mat-buf proj-loc)
                (GL13/glActiveTexture GL13/GL_TEXTURE0)
                (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
                (GL30/glBindVertexArray vao)
                (doseq [[idx pos] (map-indexed vector cube-positions)]
                  (.identity model)
                  (.translate model pos)
                  (let [angle (float (+ (* base-rotation-step idx) current))]
                    (.rotate model angle 0.5 1.0 0.2))
                  (upload-mat! model mat-buf model-loc)
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur))))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %))
            (delete-if-positive tex #(GL11/glDeleteTextures %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& [mode-arg]]
  (let [mode (keyword (or mode-arg "circle"))]
    (run-example! mode)))
