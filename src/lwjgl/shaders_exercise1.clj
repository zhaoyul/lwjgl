(ns lwjgl.shaders-exercise1
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;
out vec3 ourColor;
void main() {
    gl_Position = vec4(aPos.x, -aPos.y, aPos.z, 1.0);
    ourColor = aColor;
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec3 ourColor;
out vec4 FragColor;
void main() {
    FragColor = vec4(ourColor, 1.0);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- create-colored-triangle
  []
  (let [vertices (float-array
                  [;; positions      ;; colors
                   -0.5 -0.5 0.0     1.0 0.0 0.0
                   0.5 -0.5 0.0     0.0 1.0 0.0
                   0.0  0.5 0.0     0.0 0.0 1.0])
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
    {:vao vao :vbo vbo}))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Shaders Exercise 1 (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo]} (create-colored-triangle)]
        (try
          (GL11/glViewport 0 0 width height)
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
              (GL30/glBindVertexArray vao)
              (GL11/glDrawArrays GL11/GL_TRIANGLES 0 3)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
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
