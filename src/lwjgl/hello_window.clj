(ns lwjgl.hello-window
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11)))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Hello Window (LWJGL)")]
    (try
      (GL/createCapabilities)
      (GL11/glViewport 0 0 width height)
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
      (loop []
        (when-not (GLFW/glfwWindowShouldClose window)
          (GL11/glClearColor 0.2 0.3 0.3 1.0)
          (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
          (GLFW/glfwSwapBuffers window)
          (GLFW/glfwPollEvents)
          (recur)))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
