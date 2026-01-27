(ns lwjgl.chapter1.shaders-class
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)))

(def ^:private vertex-shader-path "shaders/shaders-class.vert")
(def ^:private fragment-shader-path "shaders/shaders-class.frag")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- create-triangle-vao
  []
  (let [vertices (float-array
                  [0.5  -0.5 0.0
                   -0.5 -0.5 0.0
                   0.0   0.5 0.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 3 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Shaders Class (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (core/create-program (core/slurp-resource vertex-shader-path)
                                         (core/slurp-resource fragment-shader-path))
            {:keys [vao vbo]} (create-triangle-vao)
            color-loc (GL20/glGetUniformLocation program "ourColor")]
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
              (let [time (GLFW/glfwGetTime)
                    green (float (+ 0.5 (/ (Math/sin time) 2.0)))]
                (GL11/glClearColor 0.2 0.3 0.3 1.0)
                (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
                (GL20/glUseProgram program)
                (when (<= 0 color-loc)
                  (GL20/glUniform4f color-loc 0.0 green 1.0 1.0))
                (GL30/glBindVertexArray vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 3)
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
