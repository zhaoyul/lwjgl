(ns lwjgl.chapter1.textures-combined
  (:gen-class)
  (:require [lwjgl.utils :as u])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL45)))

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
void main() {
    vec4 c1 = texture(texture1, TexCoord);
    vec4 c2 = texture(texture2, TexCoord);
    FragColor = mix(c1, c2, 0.5);
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
        vao (GL45/glGenVertexArrays)
        vbo (GL45/glGenBuffers)
        ebo (GL45/glGenBuffers)
        vbuf (BufferUtils/createFloatBuffer (alength vertices))
        ibuf (BufferUtils/createIntBuffer (alength indices))
        stride (* 5 Float/BYTES)]
    (GL45/glBindVertexArray vao)
    (.put vbuf vertices)
    (.flip vbuf)
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER vbuf GL45/GL_STATIC_DRAW)
    (.put ibuf indices)
    (.flip ibuf)
    (GL45/glBindBuffer GL45/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL45/glBufferData GL45/GL_ELEMENT_ARRAY_BUFFER ibuf GL45/GL_STATIC_DRAW)
    (GL45/glVertexAttribPointer 0 3 GL45/GL_FLOAT false stride 0)
    (GL45/glEnableVertexAttribArray 0)
    (GL45/glVertexAttribPointer 1 2 GL45/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL45/glEnableVertexAttribArray 1)
    (GL45/glBindVertexArray 0)
    {:vao vao :vbo vbo :ebo ebo}))

(defn- create-texture
  [w h f]
  (let [tex (GL45/glGenTextures)
        buf (BufferUtils/createByteBuffer (* w h 3))]
    (dotimes [y h]
      (dotimes [x w]
        (let [[r g b] (f x y w h)
              idx (* 3 (+ x (* y w)))]
          (.put buf idx (unchecked-byte r))
          (.put buf (inc idx) (unchecked-byte g))
          (.put buf (+ idx 2) (unchecked-byte b)))))
    (.rewind buf)
    (GL45/glBindTexture GL45/GL_TEXTURE_2D tex)
    (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_MIN_FILTER GL45/GL_LINEAR)
    (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_MAG_FILTER GL45/GL_LINEAR)
    (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_WRAP_S GL45/GL_REPEAT)
    (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_WRAP_T GL45/GL_REPEAT)
    (GL45/glTexImage2D GL45/GL_TEXTURE_2D 0 GL45/GL_RGB w h 0 GL45/GL_RGB GL45/GL_UNSIGNED_BYTE buf)
    tex))

(defn- checker
  [x y _ _]
  (let [c (if (zero? (bit-and (+ (quot x 16) (quot y 16)) 1)) 255 40)]
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
        error-callback (u/init-glfw!)
        window (u/create-window width height "LearnOpenGL - Textures Combined (LWJGL)")]
    (try
      (GL/createCapabilities)
      (let [program (u/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo ebo]} (create-quad)
            tex1 (create-texture 256 256 checker)
            tex2 (create-texture 256 256 gradient)
            tex1-loc (GL45/glGetUniformLocation program "texture1")
            tex2-loc (GL45/glGetUniformLocation program "texture2")]
        (try
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
                 (GLFW/glfwSetWindowShouldClose win true)))))
          (GL45/glUseProgram program)
          (when (<= 0 tex1-loc) (GL45/glUniform1i tex1-loc 0))
          (when (<= 0 tex2-loc) (GL45/glUniform1i tex2-loc 1))
          (loop []
            (when-not (GLFW/glfwWindowShouldClose window)
              (GL45/glClearColor 0.2 0.3 0.3 1.0)
              (GL45/glClear GL45/GL_COLOR_BUFFER_BIT)
              (GL45/glActiveTexture GL45/GL_TEXTURE0)
              (GL45/glBindTexture GL45/GL_TEXTURE_2D tex1)
              (GL45/glActiveTexture GL45/GL_TEXTURE1)
              (GL45/glBindTexture GL45/GL_TEXTURE_2D tex2)
              (GL45/glUseProgram program)
              (GL45/glBindVertexArray vao)
              (GL45/glDrawElements GL45/GL_TRIANGLES 6 GL45/GL_UNSIGNED_INT 0)
              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur)))
          (finally
            (delete-if-positive program #(GL45/glDeleteProgram %))
            (delete-if-positive vbo #(GL45/glDeleteBuffers %))
            (delete-if-positive ebo #(GL45/glDeleteBuffers %))
            (delete-if-positive vao #(GL45/glDeleteVertexArrays %))
            (delete-if-positive tex1 #(GL45/glDeleteTextures %))
            (delete-if-positive tex2 #(GL45/glDeleteTextures %)))))
      (finally
        (when (pos? window) (GLFW/glfwDestroyWindow window))
        (GLFW/glfwTerminate)
        (when error-callback (.free error-callback))))))

(defn -main
  [& _]
  (run-example!))
