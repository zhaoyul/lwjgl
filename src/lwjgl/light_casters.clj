(ns lwjgl.light-casters
  (:gen-class)
  (:require [lwjgl.core :as core]
            [lwjgl.lighting-common :as lc])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- configure-window!
  [window width height]
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
         (GLFW/glfwSetWindowShouldClose win true))))))

(defn- material-settings
  []
  {:ambient [0.2 0.2 0.2]
   :diffuse [0.5 0.5 0.5]
   :specular [1.0 1.0 1.0]
   :shininess 32.0
   :use-diffuse? false
   :use-specular? false
   :use-emission? false})

(defn- light-settings
  [mode]
  (case mode
    :directional {:type 0
                  :direction [-0.2 -1.0 -0.3]
                  :ambient [0.2 0.2 0.2]
                  :diffuse [0.5 0.5 0.5]
                  :specular [1.0 1.0 1.0]}
    :point {:type 1
            :position [1.0 1.0 2.0]
            :ambient [0.1 0.1 0.1]
            :diffuse [0.8 0.8 0.8]
            :specular [1.0 1.0 1.0]
            :constant 1.0
            :linear 0.09
            :quadratic 0.032}
    :spot {:type 2
           :position [0.0 0.0 3.0]
           :direction [0.0 0.0 -1.0]
           :ambient [0.0 0.0 0.0]
           :diffuse [1.0 1.0 1.0]
           :specular [1.0 1.0 1.0]
           :cut-off (float (Math/cos (Math/toRadians 12.5)))
           :outer-cut-off (float (Math/cos (Math/toRadians 12.5)))
           :constant 1.0
           :linear 0.09
           :quadratic 0.032}
    :spot-soft {:type 2
                :position [0.0 0.0 3.0]
                :direction [0.0 0.0 -1.0]
                :ambient [0.0 0.0 0.0]
                :diffuse [1.0 1.0 1.0]
                :specular [1.0 1.0 1.0]
                :cut-off (float (Math/cos (Math/toRadians 12.5)))
                :outer-cut-off (float (Math/cos (Math/toRadians 17.5)))
                :constant 1.0
                :linear 0.09
                :quadratic 0.032}))

(defn- set-material!
  [program {:keys [ambient diffuse specular shininess use-diffuse? use-specular? use-emission?]}]
  (let [amb (GL20/glGetUniformLocation program "material.ambient")
        diff (GL20/glGetUniformLocation program "material.diffuse")
        spec (GL20/glGetUniformLocation program "material.specular")
        shin (GL20/glGetUniformLocation program "material.shininess")
        use-d (GL20/glGetUniformLocation program "useDiffuseMap")
        use-s (GL20/glGetUniformLocation program "useSpecularMap")
        use-e (GL20/glGetUniformLocation program "useEmissionMap")]
    (when (<= 0 amb) (apply GL20/glUniform3f amb ambient))
    (when (<= 0 diff) (apply GL20/glUniform3f diff diffuse))
    (when (<= 0 spec) (apply GL20/glUniform3f spec specular))
    (when (<= 0 shin) (GL20/glUniform1f shin (float shininess)))
    (when (<= 0 use-d) (GL20/glUniform1i use-d (if use-diffuse? 1 0)))
    (when (<= 0 use-s) (GL20/glUniform1i use-s (if use-specular? 1 0)))
    (when (<= 0 use-e) (GL20/glUniform1i use-e (if use-emission? 1 0)))))

(defn- set-light!
  [program {:keys [type position direction ambient diffuse specular constant linear quadratic cut-off outer-cut-off]}]
  (let [type-loc (GL20/glGetUniformLocation program "light.type")
        pos-loc (GL20/glGetUniformLocation program "light.position")
        dir-loc (GL20/glGetUniformLocation program "light.direction")
        amb-loc (GL20/glGetUniformLocation program "light.ambient")
        diff-loc (GL20/glGetUniformLocation program "light.diffuse")
        spec-loc (GL20/glGetUniformLocation program "light.specular")
        const-loc (GL20/glGetUniformLocation program "light.constant")
        lin-loc (GL20/glGetUniformLocation program "light.linear")
        quad-loc (GL20/glGetUniformLocation program "light.quadratic")
        cut-loc (GL20/glGetUniformLocation program "light.cutOff")
        outer-loc (GL20/glGetUniformLocation program "light.outerCutOff")]
    (when (<= 0 type-loc) (GL20/glUniform1i type-loc (int type)))
    (when (and position (<= 0 pos-loc)) (apply GL20/glUniform3f pos-loc position))
    (when (and direction (<= 0 dir-loc)) (apply GL20/glUniform3f dir-loc direction))
    (when (and ambient (<= 0 amb-loc)) (apply GL20/glUniform3f amb-loc ambient))
    (when (and diffuse (<= 0 diff-loc)) (apply GL20/glUniform3f diff-loc diffuse))
    (when (and specular (<= 0 spec-loc)) (apply GL20/glUniform3f spec-loc specular))
    (when (<= 0 const-loc) (GL20/glUniform1f const-loc (float (or constant 1.0))))
    (when (<= 0 lin-loc) (GL20/glUniform1f lin-loc (float (or linear 0.09))))
    (when (<= 0 quad-loc) (GL20/glUniform1f quad-loc (float (or quadratic 0.032))))
    (when (<= 0 cut-loc) (GL20/glUniform1f cut-loc (float (or cut-off 0.0))))
    (when (<= 0 outer-loc) (GL20/glUniform1f outer-loc (float (or outer-cut-off 0.0))))))

(defn- run-mode!
  [mode]
  (let [width 800
        height 600
        {:keys [window error-callback]} (let [cb (core/init-glfw!)
                                              win (core/create-window width height (str "LearnOpenGL - " (name mode) " (LWJGL)"))]
                                          {:error-callback cb :window win})]
    (try
      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [program (lc/create-single-light-program)
            lamp-program (lc/create-lamp-program)
            {:keys [vao vbo]} (lc/create-textured-cube-mesh)
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            lamp-model (Matrix4f.)
            view (doto (Matrix4f.) (.translate (Vector3f. 0.0 0.0 -3.0)))
            projection (doto (Matrix4f.) (.perspective (float (Math/toRadians 45.0))
                                                       (/ width (float height))
                                                       0.1 100.0))
            light-pos (Vector3f. 1.2 1.0 2.0)
            model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            view-pos-loc (GL20/glGetUniformLocation program "viewPos")
            lamp-model-loc (GL20/glGetUniformLocation lamp-program "model")
            lamp-view-loc (GL20/glGetUniformLocation lamp-program "view")
            lamp-proj-loc (GL20/glGetUniformLocation lamp-program "projection")
            lamp-color-loc (GL20/glGetUniformLocation lamp-program "lightColor")]
        (try
          (configure-window! window width height)
          (GL20/glUseProgram program)
          (lc/upload-mat! view mat-buf view-loc)
          (lc/upload-mat! projection mat-buf proj-loc)
          (when (<= 0 view-pos-loc) (GL20/glUniform3f view-pos-loc 0.0 0.0 3.0))

          (GL20/glUseProgram lamp-program)
          (lc/upload-mat! view mat-buf lamp-view-loc)
          (lc/upload-mat! projection mat-buf lamp-proj-loc)

          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL11/glClearColor 0.1 0.1 0.12 1.0)
              (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

              (GL20/glUseProgram program)
              (.identity model)
              (lc/upload-mat! model mat-buf model-loc)
              (set-material! program (material-settings))
              (set-light! program (light-settings mode))
              (GL30/glBindVertexArray vao)
              (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)

              (GL20/glUseProgram lamp-program)
              (.identity lamp-model)
              (if (= mode :directional)
                (.translate lamp-model (Vector3f. -1.5 1.5 -1.5))
                (.translate lamp-model light-pos))
              (.scale lamp-model 0.2)
              (lc/upload-mat! lamp-model mat-buf lamp-model-loc)
              (when (<= 0 lamp-color-loc) (GL20/glUniform3f lamp-color-loc 1.0 1.0 1.0))
              (GL30/glBindVertexArray vao)
              (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)

              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur))))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive lamp-program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& [mode]]
  (let [mode (case mode
               "directional" :directional
               "point" :point
               "spot" :spot
               "spot-soft" :spot-soft
               :directional)]
    (run-mode! mode)))
