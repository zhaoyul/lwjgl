(ns lwjgl.chapter1.hello-window
  (:gen-class)
  (:require [lwjgl.utils :as u])
  (:import (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL45)))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (u/init-glfw!)
        window (u/create-window width height "LearnOpenGL - Hello Window (LWJGL)")]
    (try
      (GL/createCapabilities)
      (u/init-viewport! window width height)
      (GLFW/glfwSetFramebufferSizeCallback
       window
       (reify GLFWFramebufferSizeCallbackI
         (invoke [_ _ width height]
           (GL45/glViewport 0 0 width height))))
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
