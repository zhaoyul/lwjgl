(ns lwjgl.advanced-lighting
  (:gen-class)
  (:require [clojure.string :as str]
            [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL14 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(def ^:const width 800)
(def ^:const height 600)
(def ^:const depth-width 1024)
(def ^:const depth-height 1024)

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.flip buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn- create-plane
  []
  (let [vertices (float-array
                  [;; positions     ;; normals
                   -5.0 0.0 -5.0    0.0 1.0 0.0
                   5.0 0.0 -5.0     0.0 1.0 0.0
                   5.0 0.0 5.0      0.0 1.0 0.0
                   5.0 0.0 5.0      0.0 1.0 0.0
                   -5.0 0.0 5.0     0.0 1.0 0.0
                   -5.0 0.0 -5.0    0.0 1.0 0.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count 6}))

(defn- create-quad
  []
  (let [vertices (float-array
                  [;; positions   ;; tex
                   -1.0 -1.0 0.0 0.0
                   1.0 -1.0 1.0 0.0
                   1.0  1.0 1.0 1.0
                   1.0  1.0 1.0 1.0
                   -1.0  1.0 0.0 1.0
                   -1.0 -1.0 0.0 0.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 4 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride (* 2 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count 6}))

(defn- create-depth-map
  []
  (let [fbo (GL30/glGenFramebuffers)
        depth-tex (GL11/glGenTextures)]
    (GL11/glBindTexture GL11/GL_TEXTURE_2D depth-tex)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL14/GL_DEPTH_COMPONENT
                       depth-width depth-height 0 GL11/GL_DEPTH_COMPONENT GL11/GL_FLOAT
                       nil)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL13/GL_CLAMP_TO_BORDER)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL13/GL_CLAMP_TO_BORDER)
    (let [border (float-array [1.0 1.0 1.0 1.0])]
      (GL11/glTexParameterfv GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_BORDER_COLOR border))
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER fbo)
    (GL30/glFramebufferTexture2D GL30/GL_FRAMEBUFFER GL30/GL_DEPTH_ATTACHMENT GL11/GL_TEXTURE_2D depth-tex 0)
    (GL11/glDrawBuffer GL11/GL_NONE)
    (GL11/glReadBuffer GL11/GL_NONE)
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
    {:fbo fbo :depth depth-tex}))

(def ^:private depth-vs
  "#version 330 core
layout (location = 0) in vec3 aPos;
uniform mat4 model;
uniform mat4 lightSpaceMatrix;
void main() {
    gl_Position = lightSpaceMatrix * model * vec4(aPos, 1.0);
}")

(def ^:private depth-fs
  "#version 330 core
void main() {}")

(def ^:private scene-vs
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
uniform mat4 lightSpaceMatrix;
out vec3 FragPos;
out vec3 Normal;
out vec4 FragPosLightSpace;
void main() {
    FragPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    FragPosLightSpace = lightSpaceMatrix * vec4(FragPos, 1.0);
    gl_Position = projection * view * vec4(FragPos, 1.0);
}")

(def ^:private scene-fs
  "#version 330 core
in vec3 FragPos;
in vec3 Normal;
in vec4 FragPosLightSpace;
out vec4 FragColor;
uniform vec3 lightPos;
uniform vec3 viewPos;
uniform sampler2D shadowMap;
uniform int usePCF;
uniform float texelSize;

float shadowCalculation(vec4 fragPosLightSpace, vec3 lightDir, vec3 normal) {
    vec3 projCoords = fragPosLightSpace.xyz / fragPosLightSpace.w;
    projCoords = projCoords * 0.5 + 0.5;
    if (projCoords.z > 1.0) return 0.0;
    float bias = max(0.002 * (1.0 - dot(normal, lightDir)), 0.0005);
    float shadow = 0.0;
    if (usePCF == 0) {
        float closest = texture(shadowMap, projCoords.xy).r;
        shadow = projCoords.z - bias > closest ? 1.0 : 0.0;
    } else {
        for (int x = -1; x <= 1; ++x) {
            for (int y = -1; y <= 1; ++y) {
                float pcfDepth = texture(shadowMap, projCoords.xy + vec2(x, y) * texelSize).r;
                shadow += projCoords.z - bias > pcfDepth ? 1.0 : 0.0;
            }
        }
        shadow /= 9.0;
    }
    return shadow;
}

void main() {
    vec3 color = vec3(0.9, 0.85, 0.8);
    vec3 lightColor = vec3(1.0);
    vec3 ambient = 0.2 * lightColor;
    vec3 norm = normalize(Normal);
    vec3 lightDir = normalize(lightPos - FragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 diffuse = diff * lightColor;
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32.0);
    vec3 specular = 0.4 * spec * lightColor;
    float shadow = shadowCalculation(FragPosLightSpace, lightDir, norm);
    vec3 lighting = (ambient + (1.0 - shadow) * (diffuse + specular)) * color;
    FragColor = vec4(lighting, 1.0);
}")

(def ^:private debug-depth-fs
  "#version 330 core
out vec4 FragColor;
in vec2 TexCoords;
uniform sampler2D depthMap;
void main() {
    float depth = texture(depthMap, TexCoords).r;
    FragColor = vec4(vec3(depth), 1.0);
}")

(def ^:private debug-depth-vs
  "#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTex;
out vec2 TexCoords;
void main() {
    TexCoords = aTex;
    gl_Position = vec4(aPos, 0.0, 1.0);
}")

(defn- setup-window
  [title]
  (let [error-cb (core/init-glfw!)
        window (core/create-window width height title)]
    (GL/createCapabilities)
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
    {:window window :error-cb error-cb}))

(defn- cleanup-window
  [{:keys [window error-cb]}]
  (when (pos? window) (GLFW/glfwDestroyWindow window))
  (GLFW/glfwTerminate)
  (when error-cb (.free error-cb)))

(defn- draw-scene
  [{:keys [cube plane]} model model-loc mat-buf]
  (GL30/glBindVertexArray (:vao plane))
  (.identity model)
  (upload-mat! model mat-buf model-loc)
  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count plane))
  (GL30/glBindVertexArray (:vao cube))
  (doseq [[pos scale] [[[0.0 1.5 0.0] 0.5]
                       [[2.0 0.0 1.0] 0.75]
                       [[-1.5 0.0 2.0] 0.5]]]
    (.identity model)
    (.translate model (Vector3f. (float (first pos))
                                 (float (second pos))
                                 (float (nth pos 2))))
    (.scale model (float scale))
    (upload-mat! model mat-buf model-loc)
    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)))

(defn- run-shadow
  [{:keys [mode]}]
  (let [{:keys [window] :as env} (setup-window (str "LearnOpenGL - " (name mode)))
        cube (let [mesh (core/create-cube-mesh)
                   count (/ (alength core/cube-vertices) 6)]
               (assoc mesh :count count))
        plane (create-plane)
        quad (create-quad)
        depth-map (create-depth-map)
        depth-program (core/create-program depth-vs depth-fs)
        scene-program (core/create-program scene-vs scene-fs)
        debug-program (core/create-program debug-depth-vs debug-depth-fs)
        mat-buf (BufferUtils/createFloatBuffer 16)
        light-pos (Vector3f. -2.0 4.0 -1.0)
        model (Matrix4f.)
        light-space (Matrix4f.)
        projection (Matrix4f.)
        view (Matrix4f.)
        light-proj (Matrix4f.)
        light-view (Matrix4f.)]
    (try
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (GL11/glViewport 0 0 width height)
      (let [model-loc-depth (GL20/glGetUniformLocation depth-program "model")
            light-depth-loc (GL20/glGetUniformLocation depth-program "lightSpaceMatrix")
            model-loc (GL20/glGetUniformLocation scene-program "model")
            view-loc (GL20/glGetUniformLocation scene-program "view")
            proj-loc (GL20/glGetUniformLocation scene-program "projection")
            light-space-loc (GL20/glGetUniformLocation scene-program "lightSpaceMatrix")
            light-pos-loc (GL20/glGetUniformLocation scene-program "lightPos")
            view-pos-loc (GL20/glGetUniformLocation scene-program "viewPos")
            use-pcf-loc (GL20/glGetUniformLocation scene-program "usePCF")
            texel-size-loc (GL20/glGetUniformLocation scene-program "texelSize")
            debug-depth-loc (GL20/glGetUniformLocation debug-program "depthMap")]
        (GL20/glUseProgram scene-program)
        (GL20/glUniform1i (GL20/glGetUniformLocation scene-program "shadowMap") 0)
        (when (<= 0 texel-size-loc)
          (GL20/glUniform1f texel-size-loc (float (/ 1.0 depth-width))))
        (GL20/glUseProgram debug-program)
        (GL20/glUniform1i debug-depth-loc 0)
        (loop []
          (when-not (GLFW/glfwWindowShouldClose window)
            ;; light matrices
            (.ortho light-proj -10.0 10.0 -10.0 10.0 1.0 25.0)
            (.setLookAt light-view
                        (.x light-pos) (.y light-pos) (.z light-pos)
                        0.0 0.0 0.0
                        0.0 1.0 0.0)
            (.identity light-space)
            (.mul light-space light-proj)
            (.mul light-space light-view)

            ;; first pass: depth map
            (GL20/glUseProgram depth-program)
            (upload-mat! light-space mat-buf light-depth-loc)
            (GL11/glViewport 0 0 depth-width depth-height)
            (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER (:fbo depth-map))
            (GL11/glClear GL11/GL_DEPTH_BUFFER_BIT)
            (draw-scene {:cube cube :plane plane} model model-loc-depth mat-buf)
            (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)

            ;; second pass: scene render
            (GL20/glUseProgram scene-program)
            (GL11/glViewport 0 0 width height)
            (GL11/glClearColor 0.1 0.1 0.12 1.0)
            (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
            (.setPerspective projection (float (Math/toRadians 45.0)) (/ (float width) (float height)) 0.1 100.0)
            (.setLookAt view 0.0 3.0 6.0   0.0 0.0 0.0   0.0 1.0 0.0)
            (upload-mat! projection mat-buf proj-loc)
            (upload-mat! view mat-buf view-loc)
            (upload-mat! light-space mat-buf light-space-loc)
            (GL20/glUniform3f light-pos-loc (.x light-pos) (.y light-pos) (.z light-pos))
            (GL20/glUniform3f view-pos-loc 0.0 3.0 6.0)
            (GL20/glUniform1i use-pcf-loc (if (= mode :shadow-mapping) 1 0))
            (GL13/glActiveTexture GL13/GL_TEXTURE0)
            (GL11/glBindTexture GL11/GL_TEXTURE_2D (:depth depth-map))
            (draw-scene {:cube cube :plane plane} model model-loc mat-buf)

            ;; optional depth visualize
            (when (= mode :shadow-mapping-depth)
              (GL20/glUseProgram debug-program)
              (GL30/glBindVertexArray (:vao quad))
              (GL11/glDisable GL11/GL_DEPTH_TEST)
              (GL11/glBindTexture GL11/GL_TEXTURE_2D (:depth depth-map))
              (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count quad))
              (GL11/glEnable GL11/GL_DEPTH_TEST))

            (GLFW/glfwSwapBuffers window)
            (GLFW/glfwPollEvents)
            (recur)))))
      (finally
        (cleanup-window env)
        (GL20/glDeleteProgram depth-program)
        (GL20/glDeleteProgram scene-program)
        (GL20/glDeleteProgram debug-program)
        (GL15/glDeleteBuffers (:vbo cube))
        (GL30/glDeleteVertexArrays (:vao cube))
        (GL15/glDeleteBuffers (:vbo plane))
        (GL30/glDeleteVertexArrays (:vao plane))
        (GL15/glDeleteBuffers (:vbo quad))
        (GL30/glDeleteVertexArrays (:vao quad))
        (GL30/glDeleteFramebuffers (:fbo depth-map))
        (GL11/glDeleteTextures (:depth depth-map))))))

(defn run!
  ([] (run! :shadow-mapping))
  ([scenario-key]
   (let [scenario (or scenario-key :shadow-mapping)]
     (case scenario
       :shadow-mapping-depth (run-shadow {:mode :shadow-mapping-depth})
       :shadow-mapping-base (run-shadow {:mode :shadow-mapping-base})
       :shadow-mapping (run-shadow {:mode :shadow-mapping})
       ;; fall back to PCF shadow mapping for other advanced lighting items for now
       (run-shadow {:mode :shadow-mapping})))))

(defn -main
  [& args]
  (let [scenario (some-> (first args)
                         (str/replace "_" "-")
                         (keyword))]
    (run! (or scenario :shadow-mapping))))
