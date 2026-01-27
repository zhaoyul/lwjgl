(ns lwjgl.chapter2.basic-lighting-exercise3
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;

out vec3 LightingColor;

uniform vec3 lightPos;
uniform vec3 viewPos;
uniform vec3 lightColor;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    gl_Position = projection * view * model * vec4(aPos, 1.0);

    vec3 FragPos = vec3(model * vec4(aPos, 1.0));
    vec3 Normal = mat3(transpose(inverse(model))) * aNormal;

    float ambientStrength = 0.1;
    vec3 ambient = ambientStrength * lightColor;

    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(lightPos - FragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 diffuse = diff * lightColor;

    float specularStrength = 0.5;
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);
    vec3 specular = specularStrength * spec * lightColor;

    LightingColor = ambient + diffuse + specular;
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec3 LightingColor;

out vec4 FragColor;

uniform vec3 objectColor;

void main() {
    FragColor = vec4(LightingColor * objectColor, 1.0);
}")

(def ^:private lamp-vertex-shader
  "#version 330 core
layout (location = 0) in vec3 aPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    gl_Position = projection * view * model * vec4(aPos, 1.0);
}")

(def ^:private lamp-fragment-shader
  "#version 330 core
out vec4 FragColor;
uniform vec3 lightColor;
void main() {
    FragColor = vec4(lightColor, 1.0);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.flip buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Basic Lighting Exercise 3 (LWJGL)")]
    (try
      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [lighting-program (core/create-program vertex-shader-source fragment-shader-source)
            lamp-program (core/create-program lamp-vertex-shader lamp-fragment-shader)
            {:keys [vao vbo]} (core/create-cube-mesh)
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            lamp-model (Matrix4f.)
            view (doto (Matrix4f.) (.translate (Vector3f. 0.0 0.0 -3.0)))
            projection (doto (Matrix4f.) (.perspective (float (Math/toRadians 45.0))
                                                       (/ width (float height))
                                                       0.1 100.0))
            light-pos (Vector3f. 1.2 1.0 2.0)
            model-loc (GL20/glGetUniformLocation lighting-program "model")
            view-loc (GL20/glGetUniformLocation lighting-program "view")
            proj-loc (GL20/glGetUniformLocation lighting-program "projection")
            light-pos-loc (GL20/glGetUniformLocation lighting-program "lightPos")
            view-pos-loc (GL20/glGetUniformLocation lighting-program "viewPos")
            light-color-loc (GL20/glGetUniformLocation lighting-program "lightColor")
            object-color-loc (GL20/glGetUniformLocation lighting-program "objectColor")
            lamp-model-loc (GL20/glGetUniformLocation lamp-program "model")
            lamp-view-loc (GL20/glGetUniformLocation lamp-program "view")
            lamp-proj-loc (GL20/glGetUniformLocation lamp-program "projection")
            lamp-color-loc (GL20/glGetUniformLocation lamp-program "lightColor")]
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

          (GL20/glUseProgram lighting-program)
          (upload-mat! view mat-buf view-loc)
          (upload-mat! projection mat-buf proj-loc)
          (when (<= 0 object-color-loc)
            (GL20/glUniform3f object-color-loc 1.0 0.5 0.31))

          (GL20/glUseProgram lamp-program)
          (upload-mat! view mat-buf lamp-view-loc)
          (upload-mat! projection mat-buf lamp-proj-loc)

          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [t (float (GLFW/glfwGetTime))
                    light-color (Vector3f. (float (Math/sin (* 2.0 t)))
                                           (float (Math/sin (* 0.7 t)))
                                           (float (Math/sin (* 1.3 t))))]

                (GL11/glClearColor 0.1 0.1 0.12 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

                (GL20/glUseProgram lighting-program)
                (.identity model)
                (upload-mat! model mat-buf model-loc)
                (when (<= 0 light-pos-loc)
                  (GL20/glUniform3f light-pos-loc (.x light-pos) (.y light-pos) (.z light-pos)))
                (when (<= 0 view-pos-loc)
                  (GL20/glUniform3f view-pos-loc 0.0 0.0 3.0))
                (when (<= 0 light-color-loc)
                  (GL20/glUniform3f light-color-loc (.x light-color) (.y light-color) (.z light-color)))
                (GL30/glBindVertexArray vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)

                (GL20/glUseProgram lamp-program)
                (.identity lamp-model)
                (.translate lamp-model light-pos)
                (.scale lamp-model 0.2)
                (upload-mat! lamp-model mat-buf lamp-model-loc)
                (when (<= 0 lamp-color-loc)
                  (GL20/glUniform3f lamp-color-loc (.x light-color) (.y light-color) (.z light-color)))
                (GL30/glBindVertexArray vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)

                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur))))
          (finally
            (delete-if-positive lighting-program #(GL20/glDeleteProgram %))
            (delete-if-positive lamp-program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
