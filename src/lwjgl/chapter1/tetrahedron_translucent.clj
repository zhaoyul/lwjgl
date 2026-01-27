(ns lwjgl.chapter1.tetrahedron-translucent
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWMouseButtonCallbackI GLFWScrollCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)
           (org.joml Matrix4f)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
out vec3 vColor;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
    vColor = aColor;
    gl_Position = projection * view * model * vec4(aPos, 1.0);
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec3 vColor;
out vec4 FragColor;
uniform float alpha;
void main() {
    FragColor = vec4(vColor, alpha);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- clamp
  [v min-v max-v]
  (max min-v (min max-v v)))

(defn- create-tetrahedron
  []
  (let [p 0.85
        vertices (float-array
                  [;; face 1
                   p p p       0.7 0.4 0.9
                   (- p) (- p) p  0.7 0.4 0.9
                   (- p) p (- p)  0.7 0.4 0.9
                   ;; face 2
                   p p p       0.3 0.8 0.6
                   p (- p) (- p)  0.3 0.8 0.6
                   (- p) (- p) p  0.3 0.8 0.6
                   ;; face 3
                   p p p       0.9 0.6 0.3
                   (- p) p (- p)  0.9 0.6 0.3
                   p (- p) (- p)  0.9 0.6 0.3
                   ;; face 4
                   (- p) (- p) p  0.3 0.6 0.95
                   p (- p) (- p)  0.3 0.6 0.95
                   (- p) p (- p)  0.3 0.6 0.95])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count 12}))

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.rewind buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn run-example!
  []
  (let [width (atom 800)
        height (atom 600)
        yaw (atom 0.0)
        pitch (atom 0.0)
        roll (atom 0.0)
        scale (atom 0.9)
        alpha (atom 0.55)
        dragging? (atom false)
        last-x (atom 0.0)
        last-y (atom 0.0)
        last-time (atom 0.0)
        error-callback (core/init-glfw!)
        window (core/create-window @width @height "LearnOpenGL - Translucent Tetrahedron (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo count]} (create-tetrahedron)
            model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            alpha-loc (GL20/glGetUniformLocation program "alpha")
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            view (doto (Matrix4f.) (.translate 0.0 0.0 -3.2))
            projection (Matrix4f.)]
        (try
          (GL11/glEnable GL11/GL_BLEND)
          (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
          (GL11/glEnable GL11/GL_DEPTH_TEST)
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
          (GLFW/glfwSetMouseButtonCallback
           window
           (reify GLFWMouseButtonCallbackI
             (invoke [_ _ button action _]
               (when (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
                 (reset! dragging? (= action GLFW/GLFW_PRESS))))))
          (GLFW/glfwSetCursorPosCallback
           window
           (reify GLFWCursorPosCallbackI
             (invoke [_ _ xpos ypos]
               (when (or @dragging? (zero? @last-x))
                 (let [dx (- xpos @last-x)
                       dy (- ypos @last-y)]
                   (reset! last-x xpos)
                   (reset! last-y ypos)
                   (when @dragging?
                     (swap! yaw + (* 0.005 dx))
                     (swap! pitch + (* -0.005 dy))))))))
          (GLFW/glfwSetScrollCallback
           window
           (reify GLFWScrollCallbackI
             (invoke [_ _ _ yoffset]
               (swap! scale #(clamp (+ % (* 0.08 yoffset)) 0.4 2.0)))))
          (GL20/glUseProgram program)
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [now (GLFW/glfwGetTime)
                    dt (- now @last-time)
                    _ (reset! last-time now)
                    rot-speed (* 1.8 dt)
                    scale-step (* 0.8 dt)
                    new-yaw (cond-> @yaw
                              (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_A)) (+ rot-speed)
                              (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_D)) (- rot-speed))
                    new-pitch (cond-> @pitch
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_W)) (+ rot-speed)
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_S)) (- rot-speed))
                    new-roll (cond-> @roll
                               (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_Q)) (+ rot-speed)
                               (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_E)) (- rot-speed))
                    new-scale (cond-> @scale
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_Z)) (- scale-step)
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_X)) (+ scale-step))
                    new-alpha (cond-> @alpha
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_C)) (- (* 0.4 dt))
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_V)) (+ (* 0.4 dt)))]
                (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_R))
                  (reset! yaw 0.0)
                  (reset! pitch 0.0)
                  (reset! roll 0.0)
                  (reset! scale 0.9)
                  (reset! alpha 0.55))
                (reset! yaw (float new-yaw))
                (reset! pitch (float (clamp new-pitch -1.4 1.4)))
                (reset! roll (float new-roll))
                (reset! scale (float (clamp new-scale 0.4 2.0)))
                (reset! alpha (float (clamp new-alpha 0.1 0.9)))
                (.setPerspective projection (float (Math/toRadians 45.0))
                                 (/ (float @width) (float @height)) 0.1 100.0)
                (.identity model)
                (.rotateX model (float @pitch))
                (.rotateY model (float @yaw))
                (.rotateZ model (float @roll))
                (.scale model (float @scale))
                (GL11/glClearColor 0.05 0.05 0.08 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL11/glDepthMask false)
                (upload-mat! model mat-buf model-loc)
                (upload-mat! view mat-buf view-loc)
                (upload-mat! projection mat-buf proj-loc)
                (when (<= 0 alpha-loc)
                  (GL20/glUniform1f alpha-loc (float @alpha)))
                (GL30/glBindVertexArray vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 count)
                (GL11/glDepthMask true)
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur))))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
