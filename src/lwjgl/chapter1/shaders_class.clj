(ns lwjgl.chapter1.shaders-class
  (:gen-class)
  (:require [lwjgl.utils :as u])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL45)))

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
        vao (GL45/glGenVertexArrays)
        vbo (GL45/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 3 Float/BYTES)]
    (GL45/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER buf GL45/GL_STATIC_DRAW)
    (GL45/glVertexAttribPointer 0 3 GL45/GL_FLOAT false stride 0)
    (GL45/glEnableVertexAttribArray 0)
    (GL45/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (u/init-glfw!)
        window (u/create-window width height "LearnOpenGL - Shaders Class (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (u/create-program (u/slurp-resource vertex-shader-path)
                                         (u/slurp-resource fragment-shader-path))
            {:keys [vao vbo]} (create-triangle-vao)
            color-loc (GL45/glGetUniformLocation program "ourColor")]
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
              (let [time (GLFW/glfwGetTime)
                    green (float (+ 0.5 (/ (Math/sin time) 2.0)))]
                (GL45/glClearColor 0.2 0.3 0.3 1.0)
                (GL45/glClear GL45/GL_COLOR_BUFFER_BIT)
                (GL45/glUseProgram program)
                (when (<= 0 color-loc)
                  (GL45/glUniform4f color-loc 0.0 green 1.0 1.0))
                (GL45/glBindVertexArray vao)
                (GL45/glDrawArrays GL45/GL_TRIANGLES 0 3)
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
