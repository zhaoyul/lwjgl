(ns lwjgl.chapter5.advanced-lighting
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
(def ^:const position-components 3)
(def ^:const normal-components 3)
(def ^:const cube-vertex-stride (+ position-components normal-components))
(def ^:const quad-vertex-stride 4) ;; position (2) + texcoord (2)
(def ^:const pcf-texel-offset 1.0)
(def ^:const shadow-bias-scale 0.002)
(def ^:const shadow-bias-min 0.0005)

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
        stride (* cube-vertex-stride Float/BYTES)]
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
        stride (* quad-vertex-stride Float/BYTES)]
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
        depth-tex (GL11/glGenTextures)
        ^java.nio.ByteBuffer data nil]
    (GL11/glBindTexture GL11/GL_TEXTURE_2D depth-tex)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL14/GL_DEPTH_COMPONENT
                       depth-width depth-height 0 GL11/GL_DEPTH_COMPONENT GL11/GL_FLOAT
                       data)
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

;; Shared simple lighting program used by remaining Chapter 5 scenarios
(def ^:private simple-vs
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
out vec3 FragPos;
out vec3 Normal;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
    FragPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    gl_Position = projection * view * vec4(FragPos, 1.0);
}")

(def ^:private simple-fs
  "#version 330 core
in vec3 FragPos;
in vec3 Normal;
out vec4 FragColor;

uniform vec3 lightPos;
uniform vec3 viewPos;
uniform vec3 baseColor;
uniform int useBlinn;
uniform int useGamma;
uniform int useHDR;
uniform float exposure;
uniform int useBloom;
uniform float bloomThreshold;
uniform float bloomStrength;
uniform int useNormalPerturb;
uniform float normalScale;
/* parallaxMode: 0 none, 1 basic, 2 steep, 3 occlusion */
uniform int parallaxMode;
uniform float heightScale;
uniform int useSSAO;
uniform float aoStrength;
uniform int usePointLight;
uniform int useCSM;
/* deferredMode: 0 none, 1 basic, 2 volumes */
uniform int deferredMode;
uniform float csmRange;
uniform float aoBias;
uniform float gammaValue;

vec3 perturbNormal(vec3 n) {
    if (useNormalPerturb == 0) return normalize(n);
    vec3 noise = vec3(sin(FragPos.x * 3.0), 0.0, cos(FragPos.z * 3.0));
    return normalize(n + normalScale * noise);
}

float parallaxFactor() {
    if (parallaxMode == 0) return 1.0;
    float h = heightScale;
    if (parallaxMode == 2) h *= 1.5;
    if (parallaxMode == 3) h *= 2.0;
    return max(0.2, 1.0 - h);
}

vec3 applyDeferred(vec3 color, vec3 norm) {
    if (deferredMode == 0) return color;
    vec3 dir1 = normalize(vec3(-0.3, -1.0, -0.2));
    vec3 dir2 = normalize(vec3(0.3, -1.0, 0.4));
    float d1 = max(dot(norm, dir1), 0.0);
    float d2 = max(dot(norm, dir2), 0.0);
    vec3 l1 = vec3(0.2, 0.25, 0.3) * d1;
    vec3 l2 = vec3(0.2, 0.15, 0.1) * d2;
    if (deferredMode == 2) return color + 0.6 * l1 + 0.4 * l2;
    return color + 0.4 * l1 + 0.2 * l2;
}

vec3 computeLight(vec3 norm) {
    vec3 lightColor = vec3(1.0);
    vec3 lightDir = normalize(lightPos - FragPos);
    float diff = max(dot(norm, lightDir), 0.0) * parallaxFactor();
    vec3 viewDir = normalize(viewPos - FragPos);
    float spec;
    if (useBlinn == 0) {
        vec3 reflectDir = reflect(-lightDir, norm);
        spec = pow(max(dot(viewDir, reflectDir), 0.0), 16.0);
    } else {
        vec3 halfwayDir = normalize(lightDir + viewDir);
        spec = pow(max(dot(norm, halfwayDir), 0.0), 32.0);
    }
    float attenuation = 1.0;
    if (usePointLight != 0) {
        float d = length(lightPos - FragPos);
        attenuation = 1.0 / (1.0 + 0.09 * d + 0.032 * d * d);
    }
    if (useCSM != 0) {
        float slice = abs(FragPos.z);
        /* simplified cascade fade based on world-space Z distance */
        float fade = clamp(1.0 - slice / csmRange, 0.2, 1.0);
        lightColor *= fade;
    }
    vec3 ambient = 0.18 * lightColor;
    vec3 diffuse = diff * lightColor;
    vec3 specular = 0.25 * spec * lightColor;
    return (ambient + diffuse + specular) * attenuation;
}

void main() {
    vec3 norm = perturbNormal(normalize(Normal));
    vec3 lighting = computeLight(norm);
    vec3 color = lighting * baseColor;
    color = applyDeferred(color, norm);
    if (useSSAO != 0) {
        /* simplified SSAO approximation driven by normal-y */
        float occl = 1.0 - aoStrength * (1.0 - clamp(aoBias * (norm.y + 1.0), 0.0, 1.0));
        color *= occl;
    }
    if (useBloom != 0) {
        vec3 bright = max(color - vec3(bloomThreshold), vec3(0.0));
        color += bloomStrength * bright;
    }
    if (useHDR != 0) {
        color = vec3(1.0) - exp(-color * exposure);
    }
    if (useGamma != 0) {
        color = pow(color, vec3(1.0 / gammaValue));
    }
    FragColor = vec4(color, 1.0);
}")

(defn- simple-program [] (core/create-program simple-vs simple-fs))

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
  ;; bias = max(scale * (1 - dot(normal, lightDir)), minimum)
  (format "#version 330 core
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
    float bias = max(%f * (1.0 - dot(normal, lightDir)), %f);
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
}"
          shadow-bias-scale shadow-bias-min))

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

(defn- run-simple
  [mode config]
  (let [{:keys [window] :as env} (setup-window (str "LearnOpenGL - " (name mode)))
        cube (let [mesh (core/create-cube-mesh)
                   ;; cube-vertices in core.clj are pos(3)+normal(3) => stride 6
                   count (quot (alength core/cube-vertices) cube-vertex-stride)]
               (assoc mesh :count count))
        plane (create-plane)
        program (simple-program)
        mat-buf (BufferUtils/createFloatBuffer 16)
        model (Matrix4f.)
        view (Matrix4f.)
        projection (Matrix4f.)
        light-pos (Vector3f. 2.0 3.5 2.0)
        cfg (merge {:useBlinn 1
                    :useGamma 0
                    :gammaValue 2.2
                    :useHDR 0
                    :exposure 1.0
                    :useBloom 0
                    :bloomThreshold 1.0
                    :bloomStrength 0.5
                    :useNormalPerturb 0
                    :normalScale 0.0
                    :parallaxMode 0
                    :heightScale 0.0
                    :useSSAO 0
                    :aoStrength 0.3
                    :aoBias 0.5
                    :usePointLight 0
                    :useCSM 0
                    :csmRange 20.0
                    :deferredMode 0
                    :clear [0.07 0.07 0.1 1.0]}
                   config)]
    (try
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            base-color-loc (GL20/glGetUniformLocation program "baseColor")
            light-pos-loc (GL20/glGetUniformLocation program "lightPos")
            view-pos-loc (GL20/glGetUniformLocation program "viewPos")
            use-blinn-loc (GL20/glGetUniformLocation program "useBlinn")
            use-gamma-loc (GL20/glGetUniformLocation program "useGamma")
            use-hdr-loc (GL20/glGetUniformLocation program "useHDR")
            exposure-loc (GL20/glGetUniformLocation program "exposure")
            use-bloom-loc (GL20/glGetUniformLocation program "useBloom")
            bloom-thresh-loc (GL20/glGetUniformLocation program "bloomThreshold")
            bloom-strength-loc (GL20/glGetUniformLocation program "bloomStrength")
            use-normal-loc (GL20/glGetUniformLocation program "useNormalPerturb")
            normal-scale-loc (GL20/glGetUniformLocation program "normalScale")
            parallax-mode-loc (GL20/glGetUniformLocation program "parallaxMode")
            height-scale-loc (GL20/glGetUniformLocation program "heightScale")
            use-ssao-loc (GL20/glGetUniformLocation program "useSSAO")
            ao-strength-loc (GL20/glGetUniformLocation program "aoStrength")
            ao-bias-loc (GL20/glGetUniformLocation program "aoBias")
            use-point-loc (GL20/glGetUniformLocation program "usePointLight")
            use-csm-loc (GL20/glGetUniformLocation program "useCSM")
            csm-range-loc (GL20/glGetUniformLocation program "csmRange")
            deferred-mode-loc (GL20/glGetUniformLocation program "deferredMode")
            gamma-value-loc (GL20/glGetUniformLocation program "gammaValue")]
        (GL20/glUseProgram program)
        (GL20/glUniform3f light-pos-loc (.x light-pos) (.y light-pos) (.z light-pos))
        (GL20/glUniform3f view-pos-loc 0.0 3.0 6.0)
        (GL20/glUniform1i use-blinn-loc (int (:useBlinn cfg)))
        (GL20/glUniform1i use-gamma-loc (int (:useGamma cfg)))
        (GL20/glUniform1f gamma-value-loc (float (:gammaValue cfg)))
        (GL20/glUniform1i use-hdr-loc (int (:useHDR cfg)))
        (GL20/glUniform1f exposure-loc (float (:exposure cfg)))
        (GL20/glUniform1i use-bloom-loc (int (:useBloom cfg)))
        (GL20/glUniform1f bloom-thresh-loc (float (:bloomThreshold cfg)))
        (GL20/glUniform1f bloom-strength-loc (float (:bloomStrength cfg)))
        (GL20/glUniform1i use-normal-loc (int (:useNormalPerturb cfg)))
        (GL20/glUniform1f normal-scale-loc (float (:normalScale cfg)))
        (GL20/glUniform1i parallax-mode-loc (int (:parallaxMode cfg)))
        (GL20/glUniform1f height-scale-loc (float (:heightScale cfg)))
        (GL20/glUniform1i use-ssao-loc (int (:useSSAO cfg)))
        (GL20/glUniform1f ao-strength-loc (float (:aoStrength cfg)))
        (GL20/glUniform1f ao-bias-loc (float (:aoBias cfg)))
        (GL20/glUniform1i use-point-loc (int (:usePointLight cfg)))
        (GL20/glUniform1i use-csm-loc (int (:useCSM cfg)))
        (GL20/glUniform1f csm-range-loc (float (:csmRange cfg)))
        (GL20/glUniform1i deferred-mode-loc (int (:deferredMode cfg)))
        (.setPerspective projection (float (Math/toRadians 45.0)) (/ (float width) (float height)) 0.1 100.0)
        (loop []
          (when-not (GLFW/glfwWindowShouldClose window)
            (let [[r g b a] (:clear cfg)]
              (GL11/glClearColor (float r) (float g) (float b) (float a)))
            (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
            (.setLookAt view 0.0 3.0 6.0   0.0 0.5 0.0   0.0 1.0 0.0)
            (upload-mat! projection mat-buf proj-loc)
            (upload-mat! view mat-buf view-loc)

            (GL30/glBindVertexArray (:vao plane))
            (.identity model)
            (upload-mat! model mat-buf model-loc)
            (GL20/glUniform3f base-color-loc 0.7 0.7 0.72)
            (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count plane))

            (GL30/glBindVertexArray (:vao cube))
            (.identity model)
            (.translate model 0.0 0.6 -1.2)
            (.rotate model (float (GLFW/glfwGetTime)) 0.3 0.6 0.2)
            (upload-mat! model mat-buf model-loc)
            (GL20/glUniform3f base-color-loc 0.85 0.55 0.35)
            (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))

            (GLFW/glfwSwapBuffers window)
            (GLFW/glfwPollEvents)
            (recur))))
      (finally
        (cleanup-window env)
        (GL20/glDeleteProgram program)
        (GL15/glDeleteBuffers (:vbo cube))
        (GL30/glDeleteVertexArrays (:vao cube))
        (GL15/glDeleteBuffers (:vbo plane))
        (GL30/glDeleteVertexArrays (:vao plane))))))

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
    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))))

(defn- run-shadow
  [{:keys [mode]}]
  (let [{:keys [window] :as env} (setup-window (str "LearnOpenGL - " (name mode)))
        cube (let [mesh (core/create-cube-mesh)
                   ;; assumes cube-vertices are position + normal matching cube-vertex-stride
                   count (quot (alength core/cube-vertices) cube-vertex-stride)]
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
      (core/init-viewport! window width height)
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
          ;; PCF kernel offset = 1 / depth texture width
          (GL20/glUniform1f texel-size-loc (float (/ pcf-texel-offset depth-width))))
        (GL20/glUseProgram debug-program)
        (GL20/glUniform1i debug-depth-loc 0)
        (.setPerspective projection (float (Math/toRadians 45.0)) (/ (float width) (float height)) 0.1 100.0)
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
            (core/init-viewport! window width height)
            (GL11/glClearColor 0.1 0.1 0.12 1.0)
            (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
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
            (recur))))
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
       :advanced-lighting (run-simple scenario {:useBlinn 1})
       :gamma-correction (run-simple scenario {:useGamma 1})
       :point-shadows (run-simple scenario {:usePointLight 1})
       :point-shadows-soft (run-simple scenario {:usePointLight 1
                                                 :useNormalPerturb 1
                                                 :normalScale 0.1})
       :csm (run-simple scenario {:useCSM 1
                                  :clear [0.06 0.07 0.1 1.0]})
       :normal-mapping (run-simple scenario {:useNormalPerturb 1
                                             :normalScale 0.25})
       :parallax-mapping (run-simple scenario {:parallaxMode 1
                                               :heightScale 0.06})
       :steep-parallax-mapping (run-simple scenario {:parallaxMode 2}
                                           :heightScale 0.12)
       :parallax-occlusion-mapping (run-simple scenario {:parallaxMode 3
                                                         :heightScale 0.18})
       :hdr (run-simple scenario {:useHDR 1
                                  :exposure 1.6
                                  :clear [0.05 0.05 0.07 1.0]})
       :bloom (run-simple scenario {:useBloom 1
                                    :bloomThreshold 0.9
                                    :bloomStrength 0.8})
       :deferred-shading (run-simple scenario {:deferredMode 1})
       :deferred-shading-volumes (run-simple scenario {:deferredMode 2})
       :ssao (run-simple scenario {:useSSAO 1
                                   :aoStrength 0.45})
       ;; default fallback to basic lighting pass
       (run-simple scenario {})))))

(defn -main
  [& args]
  (let [scenario (some-> (first args)
                         (str/replace "_" "-")
                         (keyword))]
    (run! (or scenario :shadow-mapping))))
