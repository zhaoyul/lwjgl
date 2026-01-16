(ns lwjgl.model-loading
  (:gen-class)
  (:require [clojure.java.io :as io]
            [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.assimp Assimp AIMesh AIVector3D)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(def ^:private vertex-shader-source
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

(def ^:private fragment-shader-source
  "#version 330 core
in vec3 FragPos;
in vec3 Normal;

out vec4 FragColor;

uniform vec3 lightPos;
uniform vec3 viewPos;
uniform vec3 lightColor;
uniform vec3 objectColor;

void main() {
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

    vec3 result = (ambient + diffuse + specular) * objectColor;
    FragColor = vec4(result, 1.0);
}")

(def ^:const position-normal-floats-per-vertex 6)
(def ^:const stride-bytes (* position-normal-floats-per-vertex Float/BYTES))
(def ^:const light-frequency-x 0.5)
(def ^:const light-frequency-y 0.8)
(def ^:const light-frequency-z 1.1)
(def ^:const camera-distance 3.5)
(def ^:const object-color-r 1.0)
(def ^:const object-color-g 0.78)
(def ^:const object-color-b 0.58)

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

(defn- load-mesh!
  [^String path]
  (let [flags (bit-or Assimp/aiProcess_Triangulate
                      Assimp/aiProcess_GenSmoothNormals
                      Assimp/aiProcess_JoinIdenticalVertices
                      Assimp/aiProcess_PreTransformVertices)
        scene (Assimp/aiImportFile path flags)]
    (when (nil? scene)
      (throw (ex-info "Failed to load model" {:path path :error (Assimp/aiGetErrorString)})))
    (try
      (let [mesh-count (.mNumMeshes scene)]
        (when (zero? mesh-count)
          (throw (ex-info "No meshes in imported scene" {:path path})))
        (let [mesh (AIMesh/create (.get (.mMeshes scene) 0))
              vertices (.mVertices mesh)
              normals (.mNormals mesh)
              faces (.mFaces mesh)
              face-count (.mNumFaces mesh)
              fb (BufferUtils/createFloatBuffer (* face-count 3 position-normal-floats-per-vertex))]
          (when (nil? normals)
            (throw (ex-info "Mesh missing normals" {:path path})))
          (dotimes [i face-count]
            (let [face (.get faces i)
                  idxs (.mIndices face)
                  idx-count (.limit idxs)]
              (when-not (= idx-count 3)
                (throw (ex-info "Encountered non-triangle face" {:path path :indices idx-count})))
              (dotimes [j idx-count]
                (let [idx (.get idxs j)
                      v (.get ^AIVector3D$Buffer vertices idx)
                      n (.get ^AIVector3D$Buffer normals idx)]
                  (.put fb (.x v))
                  (.put fb (.y v))
                  (.put fb (.z v))
                  (.put fb (.x n))
                  (.put fb (.y n))
                  (.put fb (.z n))))))
          (.flip fb)
          (let [vao (GL30/glGenVertexArrays)
                vbo (GL15/glGenBuffers)]
            (GL30/glBindVertexArray vao)
            (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
            (GL15/glBufferData GL15/GL_ARRAY_BUFFER fb GL15/GL_STATIC_DRAW)
            (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride-bytes 0)
            (GL20/glEnableVertexAttribArray 0)
            (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride-bytes (* 3 Float/BYTES))
            (GL20/glEnableVertexAttribArray 1)
            (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
            (GL30/glBindVertexArray 0)
            {:vao vao
             :vbo vbo
             :vertex-count (* face-count 3)}))
        (finally
          (Assimp/aiReleaseImport scene))))))

(defn run-example!
  []
  (let [resource (io/resource "models/cube/cube.obj")
        _ (when-not resource
            (throw (ex-info "Model resource missing" {:resource "models/cube/cube.obj"})))
        model-path (if (= "file" (.getProtocol resource))
                     (.getPath resource)
                     (let [tmp-path (java.nio.file.Files/createTempFile
                                     "cube-model" ".obj"
                                     (make-array java.nio.file.attribute.FileAttribute 0))
                           tmp (.toFile tmp-path)]
                       (.deleteOnExit tmp)
                       (try
                         (with-open [in (.openStream resource)]
                           (io/copy in tmp))
                         (try
                           (java.nio.file.Files/setPosixFilePermissions
                            tmp-path
                            #{java.nio.file.attribute.PosixFilePermission/OWNER_READ
                              java.nio.file.attribute.PosixFilePermission/OWNER_WRITE})
                           (catch Exception _))
                         (.getAbsolutePath tmp)
                         (catch Exception e
                           (throw (ex-info "Failed to extract model resource"
                                           {:resource "models/cube/cube.obj"}
                                           e))))))
        width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Model Loading (LWJGL)")]
    (try
      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo vertex-count]} (load-mesh! model-path)
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            view (doto (Matrix4f.) (.translate (Vector3f. 0.0 0.0 (- camera-distance))))
            projection (doto (Matrix4f.) (.perspective (float (Math/toRadians 45.0))
                                                       (/ width (float height))
                                                       0.1 100.0))
            light-pos (Vector3f. 2.0 2.0 2.0)
            model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            light-pos-loc (GL20/glGetUniformLocation program "lightPos")
            view-pos-loc (GL20/glGetUniformLocation program "viewPos")
            light-color-loc (GL20/glGetUniformLocation program "lightColor")
            object-color-loc (GL20/glGetUniformLocation program "objectColor")]
        (try
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
                 (GLFW/glfwSetWindowShouldClose win true)))))

          (GL20/glUseProgram program)
          (upload-mat! view mat-buf view-loc)
          (upload-mat! projection mat-buf proj-loc)

          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (let [t (float (GLFW/glfwGetTime))
                    light-color (Vector3f. (float (+ 0.5 (* 0.5 (Math/sin (* light-frequency-x t)))))
                                           (float (+ 0.5 (* 0.5 (Math/sin (* light-frequency-y t)))))
                                           (float (+ 0.5 (* 0.5 (Math/sin (* light-frequency-z t))))))]
                (GL11/glClearColor 0.07 0.07 0.09 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))

                (GL20/glUseProgram program)
                (.identity model)
                (.rotateY model (* 0.4 t))
                (.rotateX model (* 0.2 t))
                (upload-mat! model mat-buf model-loc)
                (when (<= 0 light-pos-loc)
                  (GL20/glUniform3f light-pos-loc (.x light-pos) (.y light-pos) (.z light-pos)))
                (when (<= 0 view-pos-loc)
                  (GL20/glUniform3f view-pos-loc 0.0 0.0 camera-distance))
                (when (<= 0 light-color-loc)
                  (GL20/glUniform3f light-color-loc (.x light-color) (.y light-color) (.z light-color)))
                (when (<= 0 object-color-loc)
                  (GL20/glUniform3f object-color-loc object-color-r object-color-g object-color-b))

                (GL30/glBindVertexArray vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 vertex-count)

                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur))))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
