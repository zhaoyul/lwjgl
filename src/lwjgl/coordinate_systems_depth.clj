(ns lwjgl.coordinate-systems-depth
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)
           (org.joml Matrix4f Vector3f)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
out vec2 TexCoord;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
    gl_Position = projection * view * model * vec4(aPos, 1.0);
    TexCoord = aTexCoord;
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform sampler2D texture1;
void main() {
    FragColor = texture(texture1, TexCoord);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- create-cube
  []
  (let [vertices (float-array
                  [;; positions        ;; tex coords
                   -0.5 -0.5 -0.5      0.0 0.0
                    0.5 -0.5 -0.5      1.0 0.0
                    0.5  0.5 -0.5      1.0 1.0
                    0.5  0.5 -0.5      1.0 1.0
                   -0.5  0.5 -0.5      0.0 1.0
                   -0.5 -0.5 -0.5      0.0 0.0

                   -0.5 -0.5  0.5      0.0 0.0
                    0.5 -0.5  0.5      1.0 0.0
                    0.5  0.5  0.5      1.0 1.0
                    0.5  0.5  0.5      1.0 1.0
                   -0.5  0.5  0.5      0.0 1.0
                   -0.5 -0.5  0.5      0.0 0.0

                   -0.5  0.5  0.5      1.0 0.0
                   -0.5  0.5 -0.5      1.0 1.0
                   -0.5 -0.5 -0.5      0.0 1.0
                   -0.5 -0.5 -0.5      0.0 1.0
                   -0.5 -0.5  0.5      0.0 0.0
                   -0.5  0.5  0.5      1.0 0.0

                    0.5  0.5  0.5      1.0 0.0
                    0.5  0.5 -0.5      1.0 1.0
                    0.5 -0.5 -0.5      0.0 1.0
                    0.5 -0.5 -0.5      0.0 1.0
                    0.5 -0.5  0.5      0.0 0.0
                    0.5  0.5  0.5      1.0 0.0

                   -0.5 -0.5 -0.5      0.0 1.0
                    0.5 -0.5 -0.5      1.0 1.0
                    0.5 -0.5  0.5      1.0 0.0
                    0.5 -0.5  0.5      1.0 0.0
                   -0.5 -0.5  0.5      0.0 0.0
                   -0.5 -0.5 -0.5      0.0 1.0

                   -0.5  0.5 -0.5      0.0 1.0
                    0.5  0.5 -0.5      1.0 1.0
                    0.5  0.5  0.5      1.0 0.0
                    0.5  0.5  0.5      1.0 0.0
                   -0.5  0.5  0.5      0.0 0.0
                   -0.5  0.5 -0.5      0.0 1.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 5 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn- create-texture
  [w h]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [checker (if (zero? (bit-and (+ (quot x 16) (quot y 16)) 1)) 255 110)
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (byte checker))
          (.put buf (inc idx) (byte checker))
          (.put buf (+ idx 2) (byte 180)))))
    (.flip buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.flip buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(def ^:const rotation-offset 0.5)

(def cube-positions
  [(Vector3f. 0.0 0.0 0.0)
   (Vector3f. 1.2 0.0 -1.5)])

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Coordinate Systems Depth (LWJGL)")]
    (try
      (GL/createCapabilities)
      (GL11/glEnable GL11/GL_DEPTH_TEST)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo]} (create-cube)
            tex (create-texture 256 256)
            model-loc (GL20/glGetUniformLocation program "model")
            view-loc (GL20/glGetUniformLocation program "view")
            proj-loc (GL20/glGetUniformLocation program "projection")
            tex-loc (GL20/glGetUniformLocation program "texture1")
            mat-buf (BufferUtils/createFloatBuffer 16)
            model (Matrix4f.)
            view (doto (Matrix4f.) (.translate (Vector3f. 0.0 0.0 -3.0)))
            projection (doto (Matrix4f.) (.perspective (float (Math/toRadians 45.0)) (/ width (float height)) 0.1 100.0))]
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
          (when (<= 0 tex-loc) (GL20/glUniform1i tex-loc 0))
          (upload-mat! view mat-buf view-loc)
          (upload-mat! projection mat-buf proj-loc)
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL11/glClearColor 0.12 0.1 0.12 1.0)
              (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
              (GL13/glActiveTexture GL13/GL_TEXTURE0)
              (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
              (GL30/glBindVertexArray vao)
              (doseq [[idx pos] (map-indexed vector cube-positions)]
                (.identity model)
                (.translate model pos)
                (.rotate model (+ (* rotation-offset (float idx)) (float (GLFW/glfwGetTime))) 0.5 1.0 0.0)
                (upload-mat! model mat-buf model-loc)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36))
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %))
            (delete-if-positive tex #(GL11/glDeleteTextures %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
