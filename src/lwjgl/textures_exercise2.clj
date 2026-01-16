(ns lwjgl.textures-exercise2
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL13 GL15 GL20 GL30)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTexCoord;
out vec2 TexCoord;
void main() {
    gl_Position = vec4(aPos, 1.0);
    TexCoord = aTexCoord;
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform sampler2D texture1;
uniform sampler2D texture2;
uniform float mixValue;
void main() {
    vec4 c1 = texture(texture1, TexCoord);
    vec4 c2 = texture(texture2, TexCoord);
    FragColor = mix(c1, c2, mixValue);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- create-quad
  []
  (let [vertices (float-array
                  [;; positions       ;; tex coords
                   0.5  0.5  0.0      1.0 1.0
                   0.5 -0.5  0.0      1.0 0.0
                   -0.5 -0.5  0.0      0.0 0.0
                   -0.5  0.5  0.0      0.0 1.0])
        indices (int-array [0 1 3 1 2 3])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        ebo (GL15/glGenBuffers)
        vbuf (BufferUtils/createFloatBuffer (alength vertices))
        ibuf (BufferUtils/createIntBuffer (alength indices))
        stride (* 5 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put vbuf vertices)
    (.flip vbuf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER vbuf GL15/GL_STATIC_DRAW)
    (.put ibuf indices)
    (.flip ibuf)
    (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER ibuf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :ebo ebo}))

(defn- create-texture
  [w h f]
  (let [tex (GL11/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [[r g b] (f x y w h)
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (byte r))
          (.put buf (inc idx) (byte g))
          (.put buf (+ idx 2) (byte b)))))
    (.flip buf)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL11/GL_REPEAT)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL11/GL_REPEAT)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB w h 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf)
    tex))

(defn- checker
  [x y _ _]
  (let [c (if (zero? (bit-and (+ (quot x 16) (quot y 16)) 1)) 255 50)]
    [c c c]))

(defn- gradient
  [x y w h]
  (let [u (/ x (double (dec w)))
        v (/ y (double (dec h)))
        r (int (* 255 u))
        g (int (* 255 v))
        b 200]
    [r g b]))

(defn run-example!
  []
  (let [width 800
        height 600
        error-callback (core/init-glfw!)
        window (core/create-window width height "LearnOpenGL - Textures Exercise 2 (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo ebo]} (create-quad)
            tex1 (create-texture 256 256 checker)
            tex2 (create-texture 256 256 gradient)
            tex1-loc (GL20/glGetUniformLocation program "texture1")
            tex2-loc (GL20/glGetUniformLocation program "texture2")
            mix-loc (GL20/glGetUniformLocation program "mixValue")
            mix-value (atom 0.2)]
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
                 (GLFW/glfwSetWindowShouldClose win true))
               (when (#{GLFW/GLFW_PRESS GLFW/GLFW_REPEAT} action)
                 (cond
                   (= key GLFW/GLFW_KEY_UP)
                   (swap! mix-value #(min 1.0 (+ % 0.01)))
                   (= key GLFW/GLFW_KEY_DOWN)
                   (swap! mix-value #(max 0.0 (- % 0.01))))))))
          (GL20/glUseProgram program)
          (when (<= 0 tex1-loc) (GL20/glUniform1i tex1-loc 0))
          (when (<= 0 tex2-loc) (GL20/glUniform1i tex2-loc 1))
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL11/glClearColor 0.2 0.3 0.3 1.0)
              (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
              (GL13/glActiveTexture GL13/GL_TEXTURE0)
              (GL11/glBindTexture GL11/GL_TEXTURE_2D tex1)
              (GL13/glActiveTexture GL13/GL_TEXTURE1)
              (GL11/glBindTexture GL11/GL_TEXTURE_2D tex2)
              (GL20/glUseProgram program)
              (when (<= 0 mix-loc)
                (GL20/glUniform1f mix-loc (float @mix-value)))
              (GL30/glBindVertexArray vao)
              (GL11/glDrawElements GL11/GL_TRIANGLES 6 GL11/GL_UNSIGNED_INT 0)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
          (finally
            (delete-if-positive program #(GL20/glDeleteProgram %))
            (delete-if-positive vbo #(GL15/glDeleteBuffers %))
            (delete-if-positive ebo #(GL15/glDeleteBuffers %))
            (delete-if-positive vao #(GL30/glDeleteVertexArrays %))
            (delete-if-positive tex1 #(GL11/glDeleteTextures %))
            (delete-if-positive tex2 #(GL11/glDeleteTextures %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
