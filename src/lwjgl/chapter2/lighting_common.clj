(ns lwjgl.chapter2.lighting-common
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)
           (org.joml Matrix4f)))

(def textured-cube-vertices
  (float-array
   [;; positions         ;; normals          ;; tex coords
    -0.5 -0.5 -0.5  0.0 0.0 -1.0  0.0 0.0
    0.5 -0.5 -0.5   0.0 0.0 -1.0  1.0 0.0
    0.5  0.5 -0.5   0.0 0.0 -1.0  1.0 1.0
    0.5  0.5 -0.5   0.0 0.0 -1.0  1.0 1.0
    -0.5  0.5 -0.5  0.0 0.0 -1.0  0.0 1.0
    -0.5 -0.5 -0.5  0.0 0.0 -1.0  0.0 0.0

    -0.5 -0.5  0.5  0.0 0.0 1.0   0.0 0.0
    0.5 -0.5  0.5   0.0 0.0 1.0   1.0 0.0
    0.5  0.5  0.5   0.0 0.0 1.0   1.0 1.0
    0.5  0.5  0.5   0.0 0.0 1.0   1.0 1.0
    -0.5  0.5  0.5  0.0 0.0 1.0   0.0 1.0
    -0.5 -0.5  0.5  0.0 0.0 1.0   0.0 0.0

    -0.5  0.5  0.5 -1.0 0.0 0.0   1.0 0.0
    -0.5  0.5 -0.5 -1.0 0.0 0.0   1.0 1.0
    -0.5 -0.5 -0.5 -1.0 0.0 0.0   0.0 1.0
    -0.5 -0.5 -0.5 -1.0 0.0 0.0   0.0 1.0
    -0.5 -0.5  0.5 -1.0 0.0 0.0   0.0 0.0
    -0.5  0.5  0.5 -1.0 0.0 0.0   1.0 0.0

    0.5  0.5  0.5  1.0 0.0 0.0   1.0 0.0
    0.5  0.5 -0.5  1.0 0.0 0.0   1.0 1.0
    0.5 -0.5 -0.5  1.0 0.0 0.0   0.0 1.0
    0.5 -0.5 -0.5  1.0 0.0 0.0   0.0 1.0
    0.5 -0.5  0.5  1.0 0.0 0.0   0.0 0.0
    0.5  0.5  0.5  1.0 0.0 0.0   1.0 0.0

    -0.5 -0.5 -0.5 0.0 -1.0 0.0  0.0 1.0
    0.5 -0.5 -0.5  0.0 -1.0 0.0  1.0 1.0
    0.5 -0.5  0.5  0.0 -1.0 0.0  1.0 0.0
    0.5 -0.5  0.5  0.0 -1.0 0.0  1.0 0.0
    -0.5 -0.5  0.5 0.0 -1.0 0.0  0.0 0.0
    -0.5 -0.5 -0.5 0.0 -1.0 0.0  0.0 1.0

    -0.5  0.5 -0.5 0.0 1.0 0.0   0.0 1.0
    0.5  0.5 -0.5  0.0 1.0 0.0   1.0 1.0
    0.5  0.5  0.5  0.0 1.0 0.0   1.0 0.0
    0.5  0.5  0.5  0.0 1.0 0.0   1.0 0.0
    -0.5  0.5  0.5 0.0 1.0 0.0   0.0 0.0
    -0.5  0.5 -0.5 0.0 1.0 0.0   0.0 1.0]))

(defn create-textured-cube-mesh
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength textured-cube-vertices))
        stride (* 8 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf textured-cube-vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL20/glVertexAttribPointer 2 2 GL11/GL_FLOAT false stride (* 6 Float/BYTES))
    (GL20/glEnableVertexAttribArray 2)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(def single-light-vertex-shader
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoords;

out vec3 FragPos;
out vec3 Normal;
out vec2 TexCoords;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    FragPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    TexCoords = aTexCoords;
    gl_Position = projection * view * vec4(FragPos, 1.0);
}")

(def single-light-fragment-shader
  "#version 330 core
struct Material {
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float shininess;
};

struct Light {
    int type; // 0 = directional, 1 = point, 2 = spot
    vec3 position;
    vec3 direction;
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
    float constant;
    float linear;
    float quadratic;
    float cutOff;
    float outerCutOff;
};

in vec3 FragPos;
in vec3 Normal;
in vec2 TexCoords;

out vec4 FragColor;

uniform vec3 viewPos;
uniform Material material;
uniform Light light;
uniform sampler2D materialDiffuse;
uniform sampler2D materialSpecular;
uniform sampler2D emissionMap;
uniform bool useDiffuseMap;
uniform bool useSpecularMap;
uniform bool useEmissionMap;

vec3 getDiffuseColor() {
    return useDiffuseMap ? texture(materialDiffuse, TexCoords).rgb : material.diffuse;
}

vec3 getSpecularColor() {
    return useSpecularMap ? texture(materialSpecular, TexCoords).rgb : material.specular;
}

vec3 getEmission() {
    return useEmissionMap ? texture(emissionMap, TexCoords).rgb : vec3(0.0);
}

void main() {
    vec3 norm = normalize(Normal);
    vec3 lightDir = light.type == 0 ? normalize(-light.direction) : normalize(light.position - FragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 viewDir = normalize(viewPos - FragPos);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);

    float attenuation = 1.0;
    if (light.type == 1 || light.type == 2) {
        float distance = length(light.position - FragPos);
        attenuation = 1.0 / (light.constant + light.linear * distance + light.quadratic * (distance * distance));
    }

    float intensity = 1.0;
    if (light.type == 2) {
        float theta = dot(lightDir, normalize(-light.direction));
        float epsilon = max(light.cutOff - light.outerCutOff, 0.0001);
        intensity = clamp((theta - light.outerCutOff) / epsilon, 0.0, 1.0);
    }

    vec3 ambient = light.ambient * getDiffuseColor();
    vec3 diffuse = light.diffuse * diff * getDiffuseColor();
    vec3 specular = light.specular * spec * getSpecularColor();

    vec3 lighting = (ambient + intensity * (diffuse + specular)) * attenuation;
    FragColor = vec4(lighting + getEmission(), 1.0);
}")

(def lamp-vertex-shader
  "#version 330 core
layout (location = 0) in vec3 aPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
    gl_Position = projection * view * model * vec4(aPos, 1.0);
}")

(def lamp-fragment-shader
  "#version 330 core
out vec4 FragColor;
uniform vec3 lightColor;
void main() {
    FragColor = vec4(lightColor, 1.0);
}")

(defn create-single-light-program
  []
  (core/create-program single-light-vertex-shader single-light-fragment-shader))

(defn create-lamp-program
  []
  (core/create-program lamp-vertex-shader lamp-fragment-shader))

(defn upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.flip buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn create-checker-texture
  [w h [r0 g0 b0] [r1 g1 b1]]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [checker (zero? (bit-and (+ (quot x 16) (quot y 16)) 1))
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (byte (if checker r0 r1)))
          (.put buf (inc idx) (byte (if checker g0 g1)))
          (.put buf (+ idx 2) (byte (if checker b0 b1))))))
    (.flip buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))

(defn create-radial-specular-map
  [w h base highlight]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))
        cx (/ w 2.0)
        cy (/ h 2.0)
        radius (/ (min w h) 2.5)]
    (dotimes [y h]
      (dotimes [x w]
        (let [dx (- x cx)
              dy (- y cy)
              dist (Math/sqrt (+ (* dx dx) (* dy dy)))
              inside? (< dist radius)
              value (int (if inside? highlight base))
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (byte value))
          (.put buf (inc idx) (byte value))
          (.put buf (+ idx 2) (byte value)))))
    (.flip buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))

(defn create-emission-map
  [w h]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [pulse (if (zero? (bit-and (+ (quot x 8) (quot y 8)) 1)) 255 40)
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (byte pulse))
          (.put buf (inc idx) (byte 80))
          (.put buf (+ idx 2) (byte 120)))))
    (.flip buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))
