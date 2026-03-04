(ns lwjgl.chapter1.hello-triangle-indexed
  (:gen-class)
  (:require [lwjgl.utils :as u])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL45)))

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

(defn- create-quad-vao
  []
  (let [vertices (float-array
                  [0.5  0.5  0.0
                   0.5 -0.5  0.0
                   -0.5 -0.5  0.0
                   -0.5  0.5  0.0])
        indices (int-array [0 1 3
                            1 2 3])
        vao (GL45/glGenVertexArrays)
        vbo (GL45/glGenBuffers)
        ebo (GL45/glGenBuffers)
        vertex-buffer (BufferUtils/createFloatBuffer (alength vertices))
        index-buffer (BufferUtils/createIntBuffer (alength indices))]
    (GL45/glBindVertexArray vao)
    (.put vertex-buffer vertices)
    (.flip vertex-buffer)
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER vertex-buffer GL45/GL_STATIC_DRAW)

    (.put index-buffer indices)
    (.flip index-buffer)
    (GL45/glBindBuffer GL45/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL45/glBufferData GL45/GL_ELEMENT_ARRAY_BUFFER index-buffer GL45/GL_STATIC_DRAW)

    (GL45/glVertexAttribPointer 0 3 GL45/GL_FLOAT false (* 3 Float/BYTES) 0)
    (GL45/glEnableVertexAttribArray 0)
    (GL45/glBindVertexArray 0)
    {:vao vao :vbo vbo :ebo ebo}))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (u/init-glfw!)
        window (u/create-window width height "LearnOpenGL - Hello Triangle Indexed (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (u/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo ebo]} (create-quad-vao)]
        (try
          (u/init-viewport! window width height)
          (GLFW/glfwSetFramebufferSizeCallback
           window
           (reify GLFWFramebufferSizeCallbackI
             (invoke [_ _ w h]
               (GL45/glViewport 0 0 w h))))
          (GLFW/glfwSetKeyCallback
           window
           (reify GLFWKeyCallbackI
             (invoke [_ win key _ action _]
               (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                          (= action GLFW/GLFW_PRESS))
                 (GLFW/glfwSetWindowShouldClose win true)))))
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL45/glClearColor 0.2 0.3 0.3 1.0)
              (GL45/glClear GL45/GL_COLOR_BUFFER_BIT)
              (GL45/glUseProgram program)
              (GL45/glBindVertexArray vao)
              (GL45/glDrawElements GL45/GL_TRIANGLES 6 GL45/GL_UNSIGNED_INT 0)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
          (finally
            (delete-if-positive program #(GL45/glDeleteProgram %))
            (delete-if-positive ebo #(GL45/glDeleteBuffers %))
            (delete-if-positive vbo #(GL45/glDeleteBuffers %))
            (delete-if-positive vao #(GL45/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
