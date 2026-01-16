(ns lwjgl.materials
  (:gen-class)
  (:require [lwjgl.core :as core]
            [lwjgl.lighting-common :as lc])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- setup-window
  [title width height]
  (let [error-callback (core/init-glfw!)
        window (core/create-window width height title)]
    {:error-callback error-callback
     :window window}))

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

(defn- set-material-uniforms!
  [program {:keys [ambient diffuse specular shininess use-diffuse? use-specular? use-emission?]}]
  (let [amb-loc (GL20/glGetUniformLocation program "material.ambient")
        diff-loc (GL20/glGetUniformLocation program "material.diffuse")
        spec-loc (GL20/glGetUniformLocation program "material.specular")
        shin-loc (GL20/glGetUniformLocation program "material.shininess")
        use-diff-loc (GL20/glGetUniformLocation program "useDiffuseMap")
        use-spec-loc (GL20/glGetUniformLocation program "useSpecularMap")
        use-em-loc (GL20/glGetUniformLocation program "useEmissionMap")]
    (when (<= 0 amb-loc) (apply GL20/glUniform3f amb-loc ambient))
    (when (<= 0 diff-loc) (apply GL20/glUniform3f diff-loc diffuse))
    (when (<= 0 spec-loc) (apply GL20/glUniform3f spec-loc specular))
    (when (<= 0 shin-loc) (GL20/glUniform1f shin-loc (float shininess)))
    (when (<= 0 use-diff-loc) (GL20/glUniform1i use-diff-loc (if use-diffuse? 1 0)))
    (when (<= 0 use-spec-loc) (GL20/glUniform1i use-spec-loc (if use-specular? 1 0)))
    (when (<= 0 use-em-loc) (GL20/glUniform1i use-em-loc (if use-emission? 1 0)))))

(defn- set-light-uniforms!
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
        {:keys [window error-callback]} (setup-window (str "LearnOpenGL - " (name mode) " (LWJGL)") width height)]
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
          (when (<= 0 view-pos-loc)
            (GL20/glUniform3f view-pos-loc 0.0 0.0 3.0))

          (GL20/glUseProgram lamp-program)
          (lc/upload-mat! view mat-buf lamp-view-loc)
          (lc/upload-mat! projection mat-buf lamp-proj-loc)

          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [t (float (GLFW/glfwGetTime))
                    light-color (if (= mode :exercise1)
                                  (Vector3f. (float (Math/sin (* 2.0 t)))
                                             (float (Math/sin (* 0.7 t)))
                                             (float (Math/sin (* 1.3 t))))
                                  (Vector3f. 1.0 1.0 1.0))
                    light-spec (if (= mode :exercise1) [1.0 1.0 1.0] [1.0 1.0 1.0])
                    light-diff (if (= mode :exercise1)
                                 [(* 0.5 (.x light-color)) (* 0.5 (.y light-color)) (* 0.5 (.z light-color))]
                                 [0.5 0.5 0.5])
                    light-amb (if (= mode :exercise1)
                                [(* 0.2 (.x light-color)) (* 0.2 (.y light-color)) (* 0.2 (.z light-color))]
                                [0.2 0.2 0.2])
                    material (if (= mode :exercise1)
                               {:ambient [0.0 0.1 0.06]
                                :diffuse [0.0 0.6 0.3]
                                :specular [0.6 0.6 0.6]
                                :shininess 64.0
                                :use-diffuse? false
                                :use-specular? false
                                :use-emission? false}
                               {:ambient [1.0 0.5 0.31]
                                :diffuse [1.0 0.5 0.31]
                                :specular [0.5 0.5 0.5]
                                :shininess 32.0
                                :use-diffuse? false
                                :use-specular? false
                                :use-emission? false})]
                (GL11/glClearColor 0.1 0.1 0.12 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

                (GL20/glUseProgram program)
                (.identity model)
                (lc/upload-mat! model mat-buf model-loc)
                (set-material-uniforms! program material)
                (set-light-uniforms! program {:type 1
                                              :position [(.x light-pos) (.y light-pos) (.z light-pos)]
                                              :direction [0.0 0.0 -1.0]
                                              :ambient light-amb
                                              :diffuse light-diff
                                              :specular light-spec
                                              :constant 1.0
                                              :linear 0.09
                                              :quadratic 0.032})
                (GL30/glBindVertexArray vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)

                (GL20/glUseProgram lamp-program)
                (.identity lamp-model)
                (.translate lamp-model light-pos)
                (.scale lamp-model 0.2)
                (lc/upload-mat! lamp-model mat-buf lamp-model-loc)
                (when (<= 0 lamp-color-loc)
                  (GL20/glUniform3f lamp-color-loc (.x light-color) (.y light-color) (.z light-color)))
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
  (run-mode! (if (= mode "exercise1") :exercise1 :materials)))
