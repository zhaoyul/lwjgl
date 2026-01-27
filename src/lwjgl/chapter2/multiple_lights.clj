(ns lwjgl.chapter2.multiple-lights
  (:gen-class)
  (:require [lwjgl.core :as core]
            [lwjgl.chapter2.lighting-common :as lc])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(def ^:private vertex-shader
  lc/single-light-vertex-shader)

(def ^:private fragment-shader
  "#version 330 core
#define NR_POINT_LIGHTS 4

struct Material {
    float shininess;
};

struct DirLight {
    vec3 direction;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
};

struct PointLight {
    vec3 position;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float constant;
    float linear;
    float quadratic;
};

struct SpotLight {
    vec3 position;
    vec3 direction;
    float cutOff;
    float outerCutOff;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float constant;
    float linear;
    float quadratic;
};

in vec3 FragPos;
in vec3 Normal;
in vec2 TexCoords;

out vec4 FragColor;

uniform vec3 viewPos;
uniform Material material;
uniform DirLight dirLight;
uniform PointLight pointLights[NR_POINT_LIGHTS];
uniform SpotLight spotLight;
uniform sampler2D materialDiffuse;
uniform sampler2D materialSpecular;
uniform sampler2D emissionMap;
uniform bool useSpecularMap;
uniform bool useEmissionMap;

vec3 CalcDirLight(DirLight light, vec3 normal, vec3 viewDir, vec3 diffColor, vec3 specColor) {
    vec3 lightDir = normalize(-light.direction);
    float diff = max(dot(normal, lightDir), 0.0);
    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    vec3 ambient = light.ambient * diffColor;
    vec3 diffuse = light.diffuse * diff * diffColor;
    vec3 specular = light.specular * spec * specColor;
    return ambient + diffuse + specular;
}

vec3 CalcPointLight(PointLight light, vec3 normal, vec3 fragPos, vec3 viewDir, vec3 diffColor, vec3 specColor) {
    vec3 lightDir = normalize(light.position - fragPos);
    float diff = max(dot(normal, lightDir), 0.0);
    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    float distance = length(light.position - fragPos);
    float attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic * (distance * distance));
    vec3 ambient = light.ambient * diffColor;
    vec3 diffuse = light.diffuse * diff * diffColor;
    vec3 specular = light.specular * spec * specColor;
    ambient *= attenuation;
    diffuse *= attenuation;
    specular *= attenuation;
    return ambient + diffuse + specular;
}

vec3 CalcSpotLight(SpotLight light, vec3 normal, vec3 fragPos, vec3 viewDir, vec3 diffColor, vec3 specColor) {
    vec3 lightDir = normalize(light.position - fragPos);
    float diff = max(dot(normal, lightDir), 0.0);
    vec3 reflectDir = reflect(-lightDir, normal);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    float distance = length(light.position - fragPos);
    float attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic * (distance * distance));
    float theta = dot(lightDir, normalize(-light.direction));
    float epsilon = light.cutOff - light.outerCutOff;
    float intensity = clamp((theta - light.outerCutOff) / max(epsilon, 0.0001), 0.0, 1.0);
    vec3 ambient = light.ambient * diffColor;
    vec3 diffuse = light.diffuse * diff * diffColor * intensity;
    vec3 specular = light.specular * spec * specColor * intensity;
    ambient *= attenuation;
    diffuse *= attenuation;
    specular *= attenuation;
    return ambient + diffuse + specular;
}

void main() {
    vec3 norm = normalize(Normal);
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 diffuseColor = texture(materialDiffuse, TexCoords).rgb;
    vec3 specColor = useSpecularMap ? texture(materialSpecular, TexCoords).rgb : vec3(1.0);

    vec3 result = CalcDirLight(dirLight, norm, viewDir, diffuseColor, specColor);
    for(int i = 0; i < NR_POINT_LIGHTS; i++) {
        result += CalcPointLight(pointLights[i], norm, FragPos, viewDir, diffuseColor, specColor);
    }
    result += CalcSpotLight(spotLight, norm, FragPos, viewDir, diffuseColor, specColor);

    vec3 emission = useEmissionMap ? texture(emissionMap, TexCoords).rgb : vec3(0.0);
    FragColor = vec4(result + emission, 1.0);
}")

(def ^:private cube-positions
  [(Vector3f. 0.0 0.0 0.0)
   (Vector3f. 2.0 1.0 -3.0)
   (Vector3f. -1.5 -2.0 -2.5)
   (Vector3f. -3.0 1.0 -6.0)])

(def ^:private point-light-positions
  [(Vector3f. 0.7 0.2 2.0)
   (Vector3f. 2.3 3.3 -4.0)
   (Vector3f. -2.0 1.0 -3.0)
   (Vector3f. 0.0 0.0 -3.0)])

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- configure-window!
  [window width height]
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
         (GLFW/glfwSetWindowShouldClose win true))))))

(defn- set-dir-light!
  [program {:keys [direction ambient diffuse specular]}]
  (let [dir-loc (GL20/glGetUniformLocation program "dirLight.direction")
        amb-loc (GL20/glGetUniformLocation program "dirLight.ambient")
        diff-loc (GL20/glGetUniformLocation program "dirLight.diffuse")
        spec-loc (GL20/glGetUniformLocation program "dirLight.specular")]
    (when (<= 0 dir-loc) (apply GL20/glUniform3f dir-loc direction))
    (when (<= 0 amb-loc) (apply GL20/glUniform3f amb-loc ambient))
    (when (<= 0 diff-loc) (apply GL20/glUniform3f diff-loc diffuse))
    (when (<= 0 spec-loc) (apply GL20/glUniform3f spec-loc specular))))

(defn- set-point-light!
  [program idx {:keys [position ambient diffuse specular constant linear quadratic]}]
  (let [prefix (str "pointLights[" idx "].")
        position-loc (GL20/glGetUniformLocation program (str prefix "position"))
        amb-loc (GL20/glGetUniformLocation program (str prefix "ambient"))
        diff-loc (GL20/glGetUniformLocation program (str prefix "diffuse"))
        spec-loc (GL20/glGetUniformLocation program (str prefix "specular"))
        const-loc (GL20/glGetUniformLocation program (str prefix "constant"))
        lin-loc (GL20/glGetUniformLocation program (str prefix "linear"))
        quad-loc (GL20/glGetUniformLocation program (str prefix "quadratic"))]
    (when (<= 0 position-loc) (apply GL20/glUniform3f position-loc position))
    (when (<= 0 amb-loc) (apply GL20/glUniform3f amb-loc ambient))
    (when (<= 0 diff-loc) (apply GL20/glUniform3f diff-loc diffuse))
    (when (<= 0 spec-loc) (apply GL20/glUniform3f spec-loc specular))
    (when (<= 0 const-loc) (GL20/glUniform1f const-loc (float (or constant 1.0))))
    (when (<= 0 lin-loc) (GL20/glUniform1f lin-loc (float (or linear 0.09))))
    (when (<= 0 quad-loc) (GL20/glUniform1f quad-loc (float (or quadratic 0.032))))))

(defn- set-spot-light!
  [program {:keys [position direction ambient diffuse specular constant linear quadratic cut-off outer-cut-off]}]
  (let [prefix "spotLight."
        pos-loc (GL20/glGetUniformLocation program (str prefix "position"))
        dir-loc (GL20/glGetUniformLocation program (str prefix "direction"))
        amb-loc (GL20/glGetUniformLocation program (str prefix "ambient"))
        diff-loc (GL20/glGetUniformLocation program (str prefix "diffuse"))
        spec-loc (GL20/glGetUniformLocation program (str prefix "specular"))
        const-loc (GL20/glGetUniformLocation program (str prefix "constant"))
        lin-loc (GL20/glGetUniformLocation program (str prefix "linear"))
        quad-loc (GL20/glGetUniformLocation program (str prefix "quadratic"))
        cut-loc (GL20/glGetUniformLocation program (str prefix "cutOff"))
        outer-loc (GL20/glGetUniformLocation program (str prefix "outerCutOff"))]
    (when (<= 0 pos-loc) (apply GL20/glUniform3f pos-loc position))
    (when (<= 0 dir-loc) (apply GL20/glUniform3f dir-loc direction))
    (when (<= 0 amb-loc) (apply GL20/glUniform3f amb-loc ambient))
    (when (<= 0 diff-loc) (apply GL20/glUniform3f diff-loc diffuse))
    (when (<= 0 spec-loc) (apply GL20/glUniform3f spec-loc specular))
    (when (<= 0 const-loc) (GL20/glUniform1f const-loc (float (or constant 1.0))))
    (when (<= 0 lin-loc) (GL20/glUniform1f lin-loc (float (or linear 0.09))))
    (when (<= 0 quad-loc) (GL20/glUniform1f quad-loc (float (or quadratic 0.032))))
    (when (<= 0 cut-loc) (GL20/glUniform1f cut-loc (float (or cut-off 0.0))))
    (when (<= 0 outer-loc) (GL20/glUniform1f outer-loc (float (or outer-cut-off 0.0))))))

(defn- set-material!
  [program shininess use-spec-map? use-emission?]
  (let [shin-loc (GL20/glGetUniformLocation program "material.shininess")
        use-spec (GL20/glGetUniformLocation program "useSpecularMap")
        use-em (GL20/glGetUniformLocation program "useEmissionMap")]
    (when (<= 0 shin-loc) (GL20/glUniform1f shin-loc (float shininess)))
    (when (<= 0 use-spec) (GL20/glUniform1i use-spec (if use-spec-map? 1 0)))
    (when (<= 0 use-em) (GL20/glUniform1i use-em (if use-emission? 1 0)))))

(defn- point-light-configs
  [mode]
  (let [base-colors (if (= mode :exercise1)
                      [[1.0 0.6 0.0] [0.0 0.5 1.0] [0.2 1.0 0.2] [1.0 0.3 0.3]]
                      [[1.0 1.0 1.0] [1.0 1.0 1.0] [1.0 1.0 1.0] [1.0 1.0 1.0]])]
    (map (fn [pos col]
           {:position [(.x pos) (.y pos) (.z pos)]
            :ambient (mapv #(* 0.05 %) col)
            :diffuse (mapv #(* 0.8 %) col)
            :specular col
            :constant 1.0
            :linear 0.09
            :quadratic 0.032})
         point-light-positions
         base-colors)))

(defn- run-mode!
  [mode]
  (let [width 800
        height 600
        diffuse-tex (lc/create-checker-texture 128 128 [200 150 110] [120 80 60])
        specular-tex (lc/create-radial-specular-map 128 128 30 255)
        emission-tex (lc/create-emission-map 128 128)
        error-callback (core/init-glfw!)
        window (core/create-window width height (str "LearnOpenGL - multiple lights (" (name mode) ") (LWJGL)"))]
    (try
      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [program (core/create-program vertex-shader fragment-shader)
            lamp-program (lc/create-lamp-program)
            {:keys [vao vbo]} (lc/create-textured-cube-mesh)
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            lamp-model (Matrix4f.)
            view (doto (Matrix4f.) (.translate (Vector3f. 0.0 0.0 -6.0)))
            projection (doto (Matrix4f.) (.perspective (float (Math/toRadians 45.0))
                                                       (/ width (float height))
                                                       0.1 100.0))
            model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            view-pos-loc (GL20/glGetUniformLocation program "viewPos")
            lamp-model-loc (GL20/glGetUniformLocation lamp-program "model")
            lamp-view-loc (GL20/glGetUniformLocation lamp-program "view")
            lamp-proj-loc (GL20/glGetUniformLocation lamp-program "projection")
            lamp-color-loc (GL20/glGetUniformLocation lamp-program "lightColor")
            mat-diff-loc (GL20/glGetUniformLocation program "materialDiffuse")
            mat-spec-loc (GL20/glGetUniformLocation program "materialSpecular")
            emission-loc (GL20/glGetUniformLocation program "emissionMap")]
        (try
          (configure-window! window width height)

          (GL20/glUseProgram program)
          (lc/upload-mat! view mat-buf view-loc)
          (lc/upload-mat! projection mat-buf proj-loc)
          (when (<= 0 view-pos-loc) (GL20/glUniform3f view-pos-loc 0.0 0.0 6.0))
          (when (<= 0 mat-diff-loc) (GL20/glUniform1i mat-diff-loc 0))
          (when (<= 0 mat-spec-loc) (GL20/glUniform1i mat-spec-loc 1))
          (when (<= 0 emission-loc) (GL20/glUniform1i emission-loc 2))
          (set-dir-light! program {:direction [-0.2 -1.0 -0.3]
                                   :ambient [0.05 0.05 0.05]
                                   :diffuse [0.2 0.2 0.2]
                                   :specular [0.5 0.5 0.5]})
          (doseq [[idx light] (map-indexed vector (point-light-configs mode))]
            (set-point-light! program idx light))
          (set-spot-light! program {:position [0.0 0.0 6.0]
                                    :direction [0.0 0.0 -1.0]
                                    :ambient [0.0 0.0 0.0]
                                    :diffuse [1.0 1.0 1.0]
                                    :specular [1.0 1.0 1.0]
                                    :constant 1.0
                                    :linear 0.09
                                    :quadratic 0.032
                                    :cut-off (float (Math/cos (Math/toRadians 12.5)))
                                    :outer-cut-off (float (Math/cos (Math/toRadians 17.5)))})
          (set-material! program 32.0 true (= mode :exercise1))

          (GL20/glUseProgram lamp-program)
          (lc/upload-mat! view mat-buf lamp-view-loc)
          (lc/upload-mat! projection mat-buf lamp-proj-loc)

          (GL13/glActiveTexture GL13/GL_TEXTURE0)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D diffuse-tex)
          (GL13/glActiveTexture GL13/GL_TEXTURE1)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D specular-tex)
          (GL13/glActiveTexture GL13/GL_TEXTURE2)
          (GL11/glBindTexture GL11/GL_TEXTURE_2D emission-tex)

          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [t (float (GLFW/glfwGetTime))]
                (GL11/glClearColor 0.08 0.08 0.1 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

                (GL20/glUseProgram program)
                (doseq [idx (range (count cube-positions))]
                  (let [pos (nth cube-positions idx)
                        rotation (+ (* 20.0 idx) (* 25.0 t))]
                    (.identity model)
                    (.translate model pos)
                    (.rotate model (float (Math/toRadians rotation)) 0.5 1.0 0.0)
                    (lc/upload-mat! model mat-buf model-loc)
                    (GL30/glBindVertexArray vao)
                    (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)))

                (GL20/glUseProgram lamp-program)
                (doseq [pos point-light-positions]
                  (.identity lamp-model)
                  (.translate lamp-model pos)
                  (.scale lamp-model 0.15)
                  (lc/upload-mat! lamp-model mat-buf lamp-model-loc)
                  (when (<= 0 lamp-color-loc)
                    (GL20/glUniform3f lamp-color-loc 1.0 1.0 1.0))
                  (GL30/glBindVertexArray vao)
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36))

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
  (let [mode (if (= mode "exercise1") :exercise1 :multiple)]
    (run-mode! mode)))
