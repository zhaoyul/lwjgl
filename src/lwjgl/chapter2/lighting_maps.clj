(ns lwjgl.chapter2.lighting-maps
  (:gen-class)
  (:require [lwjgl.utils :as u]
            [lwjgl.chapter2.lighting-common :as lc])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL45)
           (org.joml Matrix4f Vector3f)))

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- configure-window!
  [window width height]
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
         (GLFW/glfwSetWindowShouldClose win true))))))

(defn- set-material!
  [program {:keys [ambient diffuse specular shininess use-diffuse? use-specular? use-emission?]}]
  (let [amb (GL45/glGetUniformLocation program "material.ambient")
        diff (GL45/glGetUniformLocation program "material.diffuse")
        spec (GL45/glGetUniformLocation program "material.specular")]
    (when (<= 0 amb) (apply GL45/glUniform3f amb ambient))
    (when (<= 0 diff) (apply GL45/glUniform3f diff diffuse))
    (when (<= 0 spec) (apply GL45/glUniform3f spec specular)))
  (let [shin (GL45/glGetUniformLocation program "material.shininess")
        use-d (GL45/glGetUniformLocation program "useDiffuseMap")
        use-s (GL45/glGetUniformLocation program "useSpecularMap")
        use-e (GL45/glGetUniformLocation program "useEmissionMap")]
    (when (<= 0 shin) (GL45/glUniform1f shin (float shininess)))
    (when (<= 0 use-d) (GL45/glUniform1i use-d (if use-diffuse? 1 0)))
    (when (<= 0 use-s) (GL45/glUniform1i use-s (if use-specular? 1 0)))
    (when (<= 0 use-e) (GL45/glUniform1i use-e (if use-emission? 1 0)))))

(defn- set-light!
  [program {:keys [ambient diffuse specular position]}]
  (let [type-loc (GL45/glGetUniformLocation program "light.type")
        amb-loc (GL45/glGetUniformLocation program "light.ambient")
        diff-loc (GL45/glGetUniformLocation program "light.diffuse")
        spec-loc (GL45/glGetUniformLocation program "light.specular")
        pos-loc (GL45/glGetUniformLocation program "light.position")
        const-loc (GL45/glGetUniformLocation program "light.constant")
        lin-loc (GL45/glGetUniformLocation program "light.linear")
        quad-loc (GL45/glGetUniformLocation program "light.quadratic")]
    (when (<= 0 type-loc) (GL45/glUniform1i type-loc 1))
    (when (<= 0 amb-loc) (apply GL45/glUniform3f amb-loc ambient))
    (when (<= 0 diff-loc) (apply GL45/glUniform3f diff-loc diffuse))
    (when (<= 0 spec-loc) (apply GL45/glUniform3f spec-loc specular))
    (when (<= 0 pos-loc) (apply GL45/glUniform3f pos-loc position))
    (when (<= 0 const-loc) (GL45/glUniform1f const-loc 1.0))
    (when (<= 0 lin-loc) (GL45/glUniform1f lin-loc 0.09))
    (when (<= 0 quad-loc) (GL45/glUniform1f quad-loc 0.032))))

(defn- bind-textures!
  [diffuse specular emission]
  (GL45/glActiveTexture GL45/GL_TEXTURE0)
  (GL45/glBindTexture GL45/GL_TEXTURE_2D diffuse)
  (GL45/glActiveTexture GL45/GL_TEXTURE1)
  (GL45/glBindTexture GL45/GL_TEXTURE_2D specular)
  (GL45/glActiveTexture GL45/GL_TEXTURE2)
  (GL45/glBindTexture GL45/GL_TEXTURE_2D emission))

(defn- prepare-material
  [mode]
  (case mode
    :diffuse-map {:ambient [1.0 1.0 1.0]
                  :diffuse [1.0 1.0 1.0]
                  :specular [0.5 0.5 0.5]
                  :shininess 32.0
                  :use-diffuse? true
                  :use-specular? false
                  :use-emission? false}
    :specular-map {:ambient [1.0 1.0 1.0]
                   :diffuse [1.0 1.0 1.0]
                   :specular [1.0 1.0 1.0]
                   :shininess 32.0
                   :use-diffuse? true
                   :use-specular? true
                   :use-emission? false}
    :exercise2 {:ambient [1.0 1.0 1.0]
                :diffuse [1.0 1.0 1.0]
                :specular [0.8 0.8 0.8]
                :shininess 8.0
                :use-diffuse? true
                :use-specular? true
                :use-emission? false}
    :exercise4 {:ambient [1.0 1.0 1.0]
                :diffuse [1.0 1.0 1.0]
                :specular [0.5 0.5 0.5]
                :shininess 32.0
                :use-diffuse? true
                :use-specular? true
                :use-emission? true}))

(defn- run-mode!
  [mode]
  (let [width 800
        height 600
        light-pos (Vector3f. 0.8 0.8 2.0)
        diffuse-tex (lc/create-checker-texture 128 128 [196 132 86] [120 78 48])
        specular-tex (lc/create-radial-specular-map 128 128 20 255)
        emission-tex (lc/create-emission-map 128 128)
        error-callback (u/init-glfw!)
        window (u/create-window width height (str "LearnOpenGL - lighting maps (" (name mode) ") (LWJGL)"))]
    (try
      (GL/createCapabilities)
      (GL45/glEnable GL45/GL_DEPTH_TEST)
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
            model-loc (GL45/glGetUniformLocation program "model")
            view-loc (GL45/glGetUniformLocation program "view")
            proj-loc (GL45/glGetUniformLocation program "projection")
            view-pos-loc (GL45/glGetUniformLocation program "viewPos")
            lamp-model-loc (GL45/glGetUniformLocation lamp-program "model")
            lamp-view-loc (GL45/glGetUniformLocation lamp-program "view")
            lamp-proj-loc (GL45/glGetUniformLocation lamp-program "projection")
            lamp-color-loc (GL45/glGetUniformLocation lamp-program "lightColor")
            mat-diff-loc (GL45/glGetUniformLocation program "materialDiffuse")
            mat-spec-loc (GL45/glGetUniformLocation program "materialSpecular")
            emission-loc (GL45/glGetUniformLocation program "emissionMap")]
        (try
          (configure-window! window width height)

          (GL45/glUseProgram program)
          (lc/upload-mat! view mat-buf view-loc)
          (lc/upload-mat! projection mat-buf proj-loc)
          (when (<= 0 view-pos-loc) (GL45/glUniform3f view-pos-loc 0.0 0.0 3.0))
          (when (<= 0 mat-diff-loc) (GL45/glUniform1i mat-diff-loc 0))
          (when (<= 0 mat-spec-loc) (GL45/glUniform1i mat-spec-loc 1))
          (when (<= 0 emission-loc) (GL45/glUniform1i emission-loc 2))

          (GL45/glUseProgram lamp-program)
          (lc/upload-mat! view mat-buf lamp-view-loc)
          (lc/upload-mat! projection mat-buf lamp-proj-loc)

          (bind-textures! diffuse-tex specular-tex emission-tex)

          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [t (float (GLFW/glfwGetTime))
                    rotation (if (= mode :exercise2) (* t 25.0) (* t 15.0))]
                (GL45/glClearColor 0.1 0.1 0.12 1.0)
                (GL45/glClear (bit-or GL45/GL_COLOR_BUFFER_BIT GL45/GL_DEPTH_BUFFER_BIT))

                (GL45/glUseProgram program)
                (.identity model)
                (.rotate model (float (Math/toRadians rotation)) 0.5 1.0 0.0)
                (lc/upload-mat! model mat-buf model-loc)
                (set-material! program (prepare-material mode))
                (set-light! program {:ambient [0.2 0.2 0.2]
                                     :diffuse [0.5 0.5 0.5]
                                     :specular [1.0 1.0 1.0]
                                     :position [(.x light-pos) (.y light-pos) (.z light-pos)]})
                (GL45/glBindVertexArray vao)
                (GL45/glDrawArrays GL45/GL_TRIANGLES 0 36)

                (GL45/glUseProgram lamp-program)
                (.identity lamp-model)
                (.translate lamp-model light-pos)
                (.scale lamp-model 0.2)
                (lc/upload-mat! lamp-model mat-buf lamp-model-loc)
                (when (<= 0 lamp-color-loc)
                  (GL45/glUniform3f lamp-color-loc 1.0 1.0 1.0))
                (GL45/glBindVertexArray vao)
                (GL45/glDrawArrays GL45/GL_TRIANGLES 0 36)

                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur))))
          (finally
            (delete-if-positive program #(GL45/glDeleteProgram %))
            (delete-if-positive lamp-program #(GL45/glDeleteProgram %))
            (delete-if-positive vbo #(GL45/glDeleteBuffers %))
            (delete-if-positive vao #(GL45/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& [mode]]
  (let [mode (case mode
               "specular-map" :specular-map
               "exercise2" :exercise2
               "exercise4" :exercise4
               :diffuse-map)]
    (run-mode! mode)))
