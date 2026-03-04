(ns lwjgl.chapter1.hello-point
  "渲染单个点在原点的示例"
  (:gen-class)
  (:require [lwjgl.utils :as u])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30 GL32)))

(def ^:private vertex-shader-source
  "#version 330 core
void main() {
    gl_Position = vec4(0.0, 0.0, 0.0, 1.0);
    gl_PointSize = 100.0;  // 更大的点
}")

(def ^:private fragment-shader-source
  "#version 330 core
out vec4 FragColor;
void main() {
    FragColor = vec4(1.0, 0.0, 0.0, 1.0);  // 纯红色，不使用 gl_PointCoord
}")

(defn- create-point-vao
  "创建单个点的 VAO（实际上顶点数据不会被使用）"
  []
  (let [vertices (float-array [0.0 0.0 0.0])  ; 原点
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        vertex-buffer (BufferUtils/createFloatBuffer 3)]
    (GL30/glBindVertexArray vao)
    (.put vertex-buffer vertices)
    (.flip vertex-buffer)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER vertex-buffer GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false (* 3 Float/BYTES) 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn run-example!
  []
  (let [error-callback (u/init-glfw!)
        window (u/create-window 800 600 "LearnOpenGL - Hello Point")]
    (try
      (GL/createCapabilities)
      (let [program (u/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo]} (create-point-vao)]
        (try
          #_(GL11/glEnable GL32/GL_PROGRAM_POINT_SIZE)
          (GL20/glPointSize 20)
          (u/init-viewport! window 800 600)
          (GLFW/glfwSetFramebufferSizeCallback
           window
           (reify GLFWFramebufferSizeCallbackI
             (invoke [_ _ width height]
               (GL11/glViewport 0 0 width height))))
          (GLFW/glfwSetKeyCallback
           window
           (reify GLFWKeyCallbackI
             (invoke [_ win key _ action _]
               (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                          (= action GLFW/GLFW_PRESS))
                 (GLFW/glfwSetWindowShouldClose win true)))))
          ;; 注意：OpenGL 3.3 Core Profile 默认启用程序点大小
          ;; 不需要显式调用 glEnable(GL_PROGRAM_POINT_SIZE)
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL11/glClearColor 0.2 0.3 0.3 1.0)
              (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
              (GL20/glUseProgram program)
              (GL30/glBindVertexArray vao)
              ;; 使用 GL_POINTS 模式画 1 个点
              (GL11/glDrawArrays GL11/GL_POINTS 0 1)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
          (finally
            (GL20/glDeleteProgram program)
            (GL15/glDeleteBuffers vbo)
            (GL30/glDeleteVertexArrays vao))))
      (finally
        (GLFW/glfwDestroyWindow window)
        (GLFW/glfwTerminate)
        (.free error-callback)))))

(defn -main
  [& _]
  (run-example!))
