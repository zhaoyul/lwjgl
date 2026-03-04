(ns lwjgl.chapter1.tetrahedron-input
  (:gen-class)
  (:require [lwjgl.utils :as u])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWMouseButtonCallbackI)
           (org.lwjgl.opengl GL GL45)
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
void main() {
    FragColor = vec4(vColor, 1.0);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- clamp
  [v min-v max-v]
  (max min-v (min max-v v)))

(defn- create-tetrahedron
  []
  (let [p 0.8
        vertices (float-array
                  [;; face 1
                   p p p       0.9 0.3 0.3
                   (- p) (- p) p  0.9 0.3 0.3
                   (- p) p (- p)  0.9 0.3 0.3
                   ;; face 2
                   p p p       0.3 0.9 0.4
                   p (- p) (- p)  0.3 0.9 0.4
                   (- p) (- p) p  0.3 0.9 0.4
                   ;; face 3
                   p p p       0.3 0.6 0.9
                   (- p) p (- p)  0.3 0.6 0.9
                   p (- p) (- p)  0.3 0.6 0.9
                   ;; face 4
                   (- p) (- p) p  0.9 0.8 0.3
                   p (- p) (- p)  0.9 0.8 0.3
                   (- p) p (- p)  0.9 0.8 0.3])
        vao (GL45/glGenVertexArrays)
        vbo (GL45/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 6 Float/BYTES)]
    (GL45/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER buf GL45/GL_STATIC_DRAW)
    (GL45/glVertexAttribPointer 0 3 GL45/GL_FLOAT false stride 0)
    (GL45/glEnableVertexAttribArray 0)
    (GL45/glVertexAttribPointer 1 3 GL45/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL45/glEnableVertexAttribArray 1)
    (GL45/glBindVertexArray 0)
    {:vao vao :vbo vbo :count 12}))

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.rewind buf)
  (when (<= 0 loc)
    (GL45/glUniformMatrix4fv loc false buf)))

(defn run-example!
  []
  (let [width (atom 800)
        height (atom 600)
        yaw (atom 0.0)
        pitch (atom 0.0)
        roll (atom 0.0)
        dragging? (atom false)
        last-x (atom 0.0)
        last-y (atom 0.0)
        last-time (atom 0.0)
        error-callback (u/init-glfw!)
        window (u/create-window @width @height "LearnOpenGL - Tetrahedron Input (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (u/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo count]} (create-tetrahedron)
            model-loc (GL45/glGetUniformLocation program "model")
            view-loc (GL45/glGetUniformLocation program "view")
            proj-loc (GL45/glGetUniformLocation program "projection")
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            view (doto (Matrix4f.) (.translate 0.0 0.0 -3.0))
            projection (Matrix4f.)]
        (try
          (GL45/glEnable GL45/GL_DEPTH_TEST)
          (u/init-viewport! window width height)
          (GLFW/glfwSetFramebufferSizeCallback
           window
           (reify GLFWFramebufferSizeCallbackI
             (invoke [_ _ w h]
               (reset! width w)
               (reset! height h)
               (GL45/glViewport 0 0 w h))))
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
          (GL45/glUseProgram program)
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [now (GLFW/glfwGetTime)
                    dt (- now @last-time)
                    _ (reset! last-time now)
                    rot-speed (* 1.8 dt)
                    new-yaw (cond-> @yaw
                              (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_A)) (+ rot-speed)
                              (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_D)) (- rot-speed))
                    new-pitch (cond-> @pitch
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_W)) (+ rot-speed)
                                (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_S)) (- rot-speed))
                    new-roll (cond-> @roll
                               (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_Q)) (+ rot-speed)
                               (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_E)) (- rot-speed))]
                (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_R))
                  (reset! yaw 0.0)
                  (reset! pitch 0.0)
                  (reset! roll 0.0))
                (reset! yaw (float new-yaw))
                (reset! pitch (float (clamp new-pitch -1.4 1.4)))
                (reset! roll (float new-roll))
                (.setPerspective projection (float (Math/toRadians 45.0))
                                 (/ (float @width) (float @height)) 0.1 100.0)
                (.identity model)
                (.rotateX model (float @pitch))
                (.rotateY model (float @yaw))
                (.rotateZ model (float @roll))
                (GL45/glClearColor 0.06 0.07 0.1 1.0)
                (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))
                (upload-mat! model mat-buf model-loc)
                (upload-mat! view mat-buf view-loc)
                (upload-mat! projection mat-buf proj-loc)
                (GL45/glBindVertexArray vao)
                (GL45/glDrawArrays GL45/GL_TRIANGLES 0 count)
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur))))
          (finally
            (delete-if-positive program #(GL45/glDeleteProgram %))
            (delete-if-positive vbo #(GL45/glDeleteBuffers %))
            (delete-if-positive vao #(GL45/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
