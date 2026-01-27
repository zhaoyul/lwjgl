(ns lwjgl.chapter1.hello-triangle-exercise2
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
void main() {
    gl_Position = vec4(aPos, 1.0);
}")

(def ^:private fragment-shader-source
  "#version 330 core
out vec4 FragColor;
void main() {
    FragColor = vec4(1.0, 0.5, 0.2, 1.0);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- create-two-vaos
  []
  (let [first (float-array
               [-0.9 -0.5 0.0
                -0.0 -0.5 0.0
                -0.45 0.5 0.0])
        second (float-array
                [0.0 -0.5 0.0
                 0.9 -0.5 0.0
                 0.45 0.5 0.0])
        vaos (BufferUtils/createIntBuffer 2)
        vbos (BufferUtils/createIntBuffer 2)
        stride (* 3 Float/BYTES)]
    (GL30/glGenVertexArrays vaos)
    (GL15/glGenBuffers vbos)
    (let [vao1 (.get vaos 0)
          vao2 (.get vaos 1)
          vbo1 (.get vbos 0)
          vbo2 (.get vbos 1)
          buf1 (BufferUtils/createFloatBuffer (alength first))
          buf2 (BufferUtils/createFloatBuffer (alength second))]
      ;; first triangle
      (GL30/glBindVertexArray vao1)
      (.put buf1 first)
      (.flip buf1)
      (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo1)
      (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf1 GL15/GL_STATIC_DRAW)
      (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
      (GL20/glEnableVertexAttribArray 0)
      ;; second triangle
      (GL30/glBindVertexArray vao2)
      (.put buf2 second)
      (.flip buf2)
      (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo2)
      (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf2 GL15/GL_STATIC_DRAW)
      (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
      (GL20/glEnableVertexAttribArray 0)
      (GL30/glBindVertexArray 0)
      {:vao1 vao1 :vao2 vao2 :vbo1 vbo1 :vbo2 vbo2})))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Hello Triangle Exercise 2 (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao1 vao2 vbo1 vbo2]} (create-two-vaos)]
        (try
          (core/init-viewport! window width height)
          (GLFW/glfwSetFramebufferSizeCallback
           window
           (reify GLFWFramebufferSizeCallbackI
             (invoke [_ _ w h]
               (GL11/glViewport 0 0 w h))))
          (GLFW/glfwSetKeyCallback
           window
           (reify GLFWKeyCallbackI
             (invoke [_ win key _ action _]
               (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                          (= action GLFW/GLFW_PRESS))
                 (GLFW/glfwSetWindowShouldClose win true)))))
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL11/glClearColor 0.2 0.3 0.3 1.0)
              (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
              (GL20/glUseProgram program)
              (GL30/glBindVertexArray vao1)
              (GL11/glDrawArrays GL11/GL_TRIANGLES 0 3)
              (GL30/glBindVertexArray vao2)
              (GL11/glDrawArrays GL11/GL_TRIANGLES 0 3)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo1 #(GL15/glDeleteBuffers %))
            (delete-if-positive vbo2 #(GL15/glDeleteBuffers %))
            (delete-if-positive vao1 #(GL30/glDeleteVertexArrays %))
            (delete-if-positive vao2 #(GL30/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
