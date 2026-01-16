(ns lwjgl.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [nrepl.server :as nrepl])
  (:import (org.lwjgl.glfw GLFW GLFWErrorCallback GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL12 GL13 GL15 GL20 GL30 GL31 GL33)
           (org.lwjgl BufferUtils)
           (java.util Arrays Locale Random)))

(def ^:const num-instances 10000)
(def ^:const tau (* 2.0 Math/PI))
(defonce osc-amplitude (atom 0.08))
(defonce rect-color (atom nil))
(defonce background-color (atom [0.06 0.06 0.08 1.0]))
(defonce rotation-speed (atom 1.0))
(defonce movement-speed (atom 1.0))
(defonce size-scale (atom 1.0))
(defonce wireframe? (atom false))
(defonce instancing? (atom true))
(defonce view-angles (atom {:yaw 0.0 :pitch 0.0}))
(defonce show-stats? (atom true))
(def ^:const view-angle-min -5.0)
(def ^:const view-angle-max 5.0)
(def ^:const view-angle-step 1.0)
(def ^:const overlay-tex-width 512)
(def ^:const overlay-tex-height 256)
(def ^:const font-width 5)
(def ^:const font-height 7)
(def ^:const font-scale 2)
(def ^:const font-spacing 1)
(def ^:const line-spacing 2)
(def ^:const overlay-margin 12)

(def cube-vertices
  (float-array
   [;; position xyz, normal xyz
    ;; front
    -0.5 -0.5  0.5  0.0  0.0  1.0
    0.5 -0.5  0.5  0.0  0.0  1.0
    0.5  0.5  0.5  0.0  0.0  1.0
    0.5  0.5  0.5  0.0  0.0  1.0
    -0.5  0.5  0.5  0.0  0.0  1.0
    -0.5 -0.5  0.5  0.0  0.0  1.0
    ;; back
    -0.5 -0.5 -0.5  0.0  0.0 -1.0
    -0.5  0.5 -0.5  0.0  0.0 -1.0
    0.5  0.5 -0.5  0.0  0.0 -1.0
    0.5  0.5 -0.5  0.0  0.0 -1.0
    0.5 -0.5 -0.5  0.0  0.0 -1.0
    -0.5 -0.5 -0.5  0.0  0.0 -1.0
    ;; left
    -0.5  0.5  0.5 -1.0  0.0  0.0
    -0.5  0.5 -0.5 -1.0  0.0  0.0
    -0.5 -0.5 -0.5 -1.0  0.0  0.0
    -0.5 -0.5 -0.5 -1.0  0.0  0.0
    -0.5 -0.5  0.5 -1.0  0.0  0.0
    -0.5  0.5  0.5 -1.0  0.0  0.0
    ;; right
    0.5  0.5  0.5  1.0  0.0  0.0
    0.5 -0.5 -0.5  1.0  0.0  0.0
    0.5  0.5 -0.5  1.0  0.0  0.0
    0.5 -0.5 -0.5  1.0  0.0  0.0
    0.5  0.5  0.5  1.0  0.0  0.0
    0.5 -0.5  0.5  1.0  0.0  0.0
    ;; top
    -0.5  0.5 -0.5  0.0  1.0  0.0
    -0.5  0.5  0.5  0.0  1.0  0.0
    0.5  0.5  0.5  0.0  1.0  0.0
    0.5  0.5  0.5  0.0  1.0  0.0
    0.5  0.5 -0.5  0.0  1.0  0.0
    -0.5  0.5 -0.5  0.0  1.0  0.0
    ;; bottom
    -0.5 -0.5 -0.5  0.0 -1.0  0.0
    0.5 -0.5  0.5  0.0 -1.0  0.0
    -0.5 -0.5  0.5  0.0 -1.0  0.0
    0.5 -0.5  0.5  0.0 -1.0  0.0
    -0.5 -0.5 -0.5  0.0 -1.0  0.0
    0.5 -0.5 -0.5  0.0 -1.0  0.0]))

(def ^:const font-glyphs
  {\space ["00000"
           "00000"
           "00000"
           "00000"
           "00000"
           "00000"
           "00000"]
   \. ["00000"
       "00000"
       "00000"
       "00000"
       "00000"
       "00100"
       "00100"]
   \, ["00000"
       "00000"
       "00000"
       "00000"
       "00100"
       "00100"
       "01000"]
   \: ["00000"
       "00100"
       "00100"
       "00000"
       "00100"
       "00100"
       "00000"]
   \[ ["01110"
       "01000"
       "01000"
       "01000"
       "01000"
       "01000"
       "01110"]
   \] ["01110"
       "00010"
       "00010"
       "00010"
       "00010"
       "00010"
       "01110"]
   \? ["01110"
       "10001"
       "00010"
       "00100"
       "00100"
       "00000"
       "00100"]
   \0 ["01110"
       "10001"
       "10011"
       "10101"
       "11001"
       "10001"
       "01110"]
   \1 ["00100"
       "01100"
       "00100"
       "00100"
       "00100"
       "00100"
       "01110"]
   \2 ["01110"
       "10001"
       "00001"
       "00010"
       "00100"
       "01000"
       "11111"]
   \3 ["11110"
       "00001"
       "00001"
       "01110"
       "00001"
       "00001"
       "11110"]
   \4 ["00010"
       "00110"
       "01010"
       "10010"
       "11111"
       "00010"
       "00010"]
   \5 ["11111"
       "10000"
       "11110"
       "00001"
       "00001"
       "10001"
       "01110"]
   \6 ["00110"
       "01000"
       "10000"
       "11110"
       "10001"
       "10001"
       "01110"]
   \7 ["11111"
       "00001"
       "00010"
       "00100"
       "01000"
       "01000"
       "01000"]
   \8 ["01110"
       "10001"
       "10001"
       "01110"
       "10001"
       "10001"
       "01110"]
   \9 ["01110"
       "10001"
       "10001"
       "01111"
       "00001"
       "00010"
       "01100"]
   \A ["01110"
       "10001"
       "10001"
       "11111"
       "10001"
       "10001"
       "10001"]
   \B ["11110"
       "10001"
       "10001"
       "11110"
       "10001"
       "10001"
       "11110"]
   \C ["01110"
       "10001"
       "10000"
       "10000"
       "10000"
       "10001"
       "01110"]
   \D ["11110"
       "10001"
       "10001"
       "10001"
       "10001"
       "10001"
       "11110"]
   \E ["11111"
       "10000"
       "10000"
       "11110"
       "10000"
       "10000"
       "11111"]
   \F ["11111"
       "10000"
       "10000"
       "11110"
       "10000"
       "10000"
       "10000"]
   \G ["01110"
       "10001"
       "10000"
       "10111"
       "10001"
       "10001"
       "01110"]
   \I ["01110"
       "00100"
       "00100"
       "00100"
       "00100"
       "00100"
       "01110"]
   \L ["10000"
       "10000"
       "10000"
       "10000"
       "10000"
       "10000"
       "11111"]
   \M ["10001"
       "11011"
       "10101"
       "10001"
       "10001"
       "10001"
       "10001"]
   \N ["10001"
       "11001"
       "10101"
       "10011"
       "10001"
       "10001"
       "10001"]
   \O ["01110"
       "10001"
       "10001"
       "10001"
       "10001"
       "10001"
       "01110"]
   \P ["11110"
       "10001"
       "10001"
       "11110"
       "10000"
       "10000"
       "10000"]
   \R ["11110"
       "10001"
       "10001"
       "11110"
       "10100"
       "10010"
       "10001"]
   \S ["01111"
       "10000"
       "10000"
       "01110"
       "00001"
       "00001"
       "11110"]
   \T ["11111"
       "00100"
       "00100"
       "00100"
       "00100"
       "00100"
       "00100"]
   \U ["10001"
       "10001"
       "10001"
       "10001"
       "10001"
       "10001"
       "01110"]
   \V ["10001"
       "10001"
       "10001"
       "10001"
       "10001"
       "01010"
       "00100"]
   \W ["10001"
       "10001"
       "10001"
       "10101"
       "10101"
       "10101"
       "01010"]
   \X ["10001"
       "10001"
       "01010"
       "00100"
       "01010"
       "10001"
       "10001"]})

(defn glyph-pattern
  [ch]
  (or (get font-glyphs ch)
      (get font-glyphs (Character/toUpperCase ch))
      (get font-glyphs \?)))

(defn clear-text-buffer!
  [^bytes buf]
  (Arrays/fill buf (byte 0)))

(defn set-text-pixel!
  [^bytes buf tex-w tex-h x y]
  (when (and (<= 0 x) (< x tex-w) (<= 0 y) (< y tex-h))
    (aset-byte buf (+ x (* y tex-w)) (byte -1))))

(defn draw-glyph!
  [^bytes buf tex-w tex-h x y pattern]
  (dotimes [row font-height]
    (let [^String line (nth pattern row)]
      (dotimes [col font-width]
        (when (= \1 (.charAt line col))
          (let [px (+ x (* col font-scale))
                py (+ y (* row font-scale))]
            (dotimes [sy font-scale]
              (let [yy (+ py sy)]
                (dotimes [sx font-scale]
                  (set-text-pixel! buf tex-w tex-h (+ px sx) yy))))))))))

(defn draw-string!
  [^bytes buf tex-w tex-h x y ^String s]
  (let [advance (+ (* font-width font-scale) (* font-spacing font-scale))]
    (dotimes [idx (.length s)]
      (let [pattern (glyph-pattern (.charAt s idx))]
        (draw-glyph! buf tex-w tex-h (+ x (* idx advance)) y pattern)))))

(defn text-size
  [lines]
  (let [line-width (fn [^String line]
                     (let [n (.length line)]
                       (+ (* n font-width font-scale)
                          (* (max 0 (dec n)) font-spacing font-scale))))
        widths (map line-width lines)
        w (if (seq widths) (apply max widths) 0)
        n (count lines)
        h (if (pos? n)
            (+ (* n font-height font-scale)
               (* (dec n) line-spacing font-scale))
            0)]
    {:w w :h h}))

(defn rand-range
  ^double
  [^Random rng ^double min-val ^double max-val]
  (+ min-val (* (- max-val min-val) (.nextFloat rng))))

(defn clamp
  ^double
  [^double v ^double min-val ^double max-val]
  (-> v (max min-val) (min max-val)))

(defn init-instance-state
  [num]
  (let [rng (Random.)
        base (float-array (* num 3))
        params (float-array (* num 4))
        sizes (float-array num)
        instance-data (float-array (* num 4))]
    (loop [i 0
           b 0
           p 0
           s 0]
      (when (< i num)
        (let [x (rand-range rng -0.9 0.9)
              y (rand-range rng -0.9 0.9)
              z (rand-range rng -0.4 0.4)
              size (rand-range rng 0.01 0.05)
              fx (rand-range rng 0.4 1.4)
              fy (rand-range rng 0.4 1.4)
              phx (rand-range rng 0.0 tau)
              phy (rand-range rng 0.0 tau)]
          (aset base b (float x))
          (aset base (unchecked-add-int b 1) (float y))
          (aset base (unchecked-add-int b 2) (float z))
          (aset sizes s (float size))
          (aset params p (float fx))
          (aset params (unchecked-add-int p 1) (float fy))
          (aset params (unchecked-add-int p 2) (float phx))
          (aset params (unchecked-add-int p 3) (float phy)))
        (recur (unchecked-inc-int i)
               (unchecked-add-int b 3)
               (unchecked-add-int p 4)
               (unchecked-inc-int s))))
    {:base base
     :params params
     :sizes sizes
     :instance-data instance-data}))

(defn update-instance-data!
  [base params sizes instance-data t osc-amplitude movement-speed size-scale]
  (let [^floats base base
        ^floats params params
        ^floats sizes sizes
        ^floats instance-data instance-data
        ^double t t
        ^double osc-amplitude osc-amplitude
        ^double movement-speed movement-speed
        ^double size-scale size-scale
        num (quot (alength base) 3)]
    (loop [i 0
           b 0
           p 0
           s 0
           inst 0]
      (when (< i num)
        (let [bx (aget base b)
              by (aget base (unchecked-add-int b 1))
              bz (aget base (unchecked-add-int b 2))
              fx (aget params p)
              fy (aget params (unchecked-add-int p 1))
              phx (aget params (unchecked-add-int p 2))
              phy (aget params (unchecked-add-int p 3))
              size (aget sizes s)
              x (+ bx (* osc-amplitude (Math/sin (+ (* fx t movement-speed) phx))))
              y (+ by (* osc-amplitude (Math/sin (+ (* fy t movement-speed) phy))))]
          (aset instance-data inst (float x))
          (aset instance-data (unchecked-add-int inst 1) (float y))
          (aset instance-data (unchecked-add-int inst 2) (float bz))
          (aset instance-data (unchecked-add-int inst 3) (float (* size size-scale)))
          (recur (unchecked-inc-int i)
                 (unchecked-add-int b 3)
                 (unchecked-add-int p 4)
                 (unchecked-inc-int s)
                 (unchecked-add-int inst 4)))))))

(defn slurp-resource
  [path]
  (slurp (io/resource path)))

(defn start-nrepl!
  []
  (let [middleware (try
                     (require 'cider.nrepl)
                     (when-let [mw (resolve 'cider.nrepl/cider-middleware)]
                       (seq (var-get mw)))
                     (catch Throwable _ nil))
        handler (if middleware
                  (apply nrepl/default-handler middleware)
                  (nrepl/default-handler))
        server (nrepl/start-server :port 0 :handler handler)
        port (:port server)]
    (spit ".nrepl-port" (str port))
    (println "nREPL listening on port" port)
    server))

(defn stop-nrepl!
  [server]
  (when server
    (nrepl/stop-server server)
    (io/delete-file ".nrepl-port" true)))

(defn compile-shader
  [shader-type source]
  (let [shader (GL20/glCreateShader shader-type)]
    (GL20/glShaderSource shader source)
    (GL20/glCompileShader shader)
    (when (zero? (GL20/glGetShaderi shader GL20/GL_COMPILE_STATUS))
      (let [log (GL20/glGetShaderInfoLog shader)]
        (GL20/glDeleteShader shader)
        (throw (RuntimeException. (str "Shader compile failed: " log)))))
    shader))

(defn create-program
  [vs-source fs-source]
  (let [vs (compile-shader GL20/GL_VERTEX_SHADER vs-source)
        fs (compile-shader GL20/GL_FRAGMENT_SHADER fs-source)
        program (GL20/glCreateProgram)]
    (GL20/glAttachShader program vs)
    (GL20/glAttachShader program fs)
    (GL20/glLinkProgram program)
    (when (zero? (GL20/glGetProgrami program GL20/GL_LINK_STATUS))
      (let [log (GL20/glGetProgramInfoLog program)]
        (GL20/glDeleteProgram program)
        (throw (RuntimeException. (str "Program link failed: " log)))))
    (GL20/glDeleteShader vs)
    (GL20/glDeleteShader fs)
    program))

(defn create-cube-mesh
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength cube-vertices))]
    (GL30/glBindVertexArray vao)
    (.put buf cube-vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (let [stride (* 6 Float/BYTES)]
      (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
      (GL20/glEnableVertexAttribArray 0)
      (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
      (GL20/glEnableVertexAttribArray 1))
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn create-instance-buffer
  [num-instances]
  (let [vbo (GL15/glGenBuffers)
        stride (* 4 Float/BYTES)]
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER
                       (* num-instances 4 Float/BYTES)
                       GL15/GL_STREAM_DRAW)
    (GL20/glVertexAttribPointer 2 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 2)
    (GL20/glVertexAttribPointer 3 1 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 3)
    (GL33/glVertexAttribDivisor 2 1)
    (GL33/glVertexAttribDivisor 3 1)
    vbo))

(defn create-overlay-texture
  [w h]
  (let [tex (GL11/glGenTextures)]
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glPixelStorei GL11/GL_UNPACK_ALIGNMENT 1)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_NEAREST)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_S GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_WRAP_T GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL30/GL_R8 w h 0
                       GL11/GL_RED GL11/GL_UNSIGNED_BYTE nil)
    tex))

(defn create-overlay-quad
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        stride (* 4 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER (* 6 4 Float/BYTES) GL15/GL_STREAM_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride (* 2 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn update-overlay!
  [overlay lines]
  (let [{:keys [tex tex-w tex-h bytes buffer vbo vertex-buffer]} overlay
        {:keys [w h]} (text-size lines)
        w (min w tex-w)
        h (min h tex-h)]
    (clear-text-buffer! bytes)
    (loop [ys 0
           remaining lines]
      (when (seq remaining)
        (draw-string! bytes tex-w tex-h 0 ys (first remaining))
        (recur (+ ys (* font-height font-scale) (* line-spacing font-scale))
               (rest remaining))))
    (.clear buffer)
    (.put buffer bytes 0 (* tex-w tex-h))
    (.flip buffer)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexSubImage2D GL11/GL_TEXTURE_2D 0 0 0 tex-w tex-h
                          GL11/GL_RED GL11/GL_UNSIGNED_BYTE buffer)
    (let [x0 (float overlay-margin)
          y0 (float overlay-margin)
          x1 (float (+ overlay-margin w))
          y1 (float (+ overlay-margin h))
          u-left 0.0
          u-right (float (/ w (double tex-w)))
          v-top 0.0
          v-bottom (float (/ h (double tex-h)))]
      (.clear vertex-buffer)
      (.put vertex-buffer (float-array
                           [x0 y0 u-left v-top
                            x1 y0 u-right v-top
                            x1 y1 u-right v-bottom
                            x1 y1 u-right v-bottom
                            x0 y1 u-left v-bottom
                            x0 y0 u-left v-top]))
      (.flip vertex-buffer)
      (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
      (GL15/glBufferSubData GL15/GL_ARRAY_BUFFER 0 vertex-buffer))))

(defn format-int
  [n]
  (String/format Locale/US "%,d" (object-array [(long n)])))

(defn format-ms
  [v]
  (String/format Locale/US "%.2f" (object-array [(double v)])))

(defn format-fps
  [v]
  (String/format Locale/US "%.1f" (object-array [(double v)])))

(defn init-glfw!
  []
  (let [error-callback (GLFWErrorCallback/createPrint System/err)]
    (.set error-callback)
    (when-not (GLFW/glfwInit)
      (throw (IllegalStateException. "Unable to initialize GLFW")))
    error-callback))

(defn init-viewport!
  [window width height]
  (let [w (BufferUtils/createIntBuffer 1)
        h (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetFramebufferSize window w h)
    (let [fw (.get w 0)
          fh (.get h 0)]
      (reset! width fw)
      (reset! height fh)
      (GL11/glViewport 0 0 fw fh))))

(defn create-window
  [width height title]
  (GLFW/glfwDefaultWindowHints)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (when (= (System/getProperty "os.name") "Mac OS X")
    (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL11/GL_TRUE))
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GL11/GL_TRUE)
  (let [window (GLFW/glfwCreateWindow width height title 0 0)]
    (when (zero? window)
      (throw (RuntimeException. "Failed to create GLFW window")))
    (GLFW/glfwMakeContextCurrent window)
    (GLFW/glfwSwapInterval 1)
    (GLFW/glfwShowWindow window)
    window))

(defn cleanup!
  [window vao vbo instance-vbo program overlay-vao overlay-vbo overlay-tex overlay-program
   error-callback gl-ready? nrepl-server]
  (when gl-ready?
    (GL20/glUseProgram 0)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
    (GL30/glBindVertexArray 0)
    (when (pos? vbo) (GL15/glDeleteBuffers vbo))
    (when (pos? instance-vbo) (GL15/glDeleteBuffers instance-vbo))
    (when (pos? overlay-vbo) (GL15/glDeleteBuffers overlay-vbo))
    (when (pos? vao) (GL30/glDeleteVertexArrays vao))
    (when (pos? overlay-vao) (GL30/glDeleteVertexArrays overlay-vao))
    (when (pos? program) (GL20/glDeleteProgram program))
    (when (pos? overlay-program) (GL20/glDeleteProgram overlay-program))
    (when (pos? overlay-tex) (GL11/glDeleteTextures overlay-tex)))
  (stop-nrepl! nrepl-server)
  (when (pos? window) (GLFW/glfwDestroyWindow window))
  (GLFW/glfwTerminate)
  (.free error-callback))

(defn run-app!
  []
  (let [error-callback (init-glfw!)
        width (atom 1200)
        height (atom 800)
        window (create-window @width @height "LWJGL Cubes")
        gl-ready? (atom false)
        nrepl-server (atom nil)
        last-time (atom 0.0)
        fps-state (atom {:acc 0.0 :frames 0 :fps 0.0})]
    (try
      (GL/createCapabilities)
      (reset! gl-ready? true)
      (reset! nrepl-server (start-nrepl!))
      (println "GL_VENDOR:" (GL11/glGetString GL11/GL_VENDOR))
      (println "GL_RENDERER:" (GL11/glGetString GL11/GL_RENDERER))
      (println "GL_VERSION:" (GL11/glGetString GL11/GL_VERSION))
      (println "GLSL_VERSION:" (GL11/glGetString GL20/GL_SHADING_LANGUAGE_VERSION))
      (init-viewport! window width height)
      (GL11/glDisable GL11/GL_DEPTH_TEST)

      (GLFW/glfwSetFramebufferSizeCallback
       window
       (reify GLFWFramebufferSizeCallbackI
         (invoke [_ _ w h]
           (reset! width w)
           (reset! height h)
           (GL11/glViewport 0 0 w h))))

      (GLFW/glfwSetKeyCallback
       window
       (reify GLFWKeyCallbackI
         (invoke [_ win key _ action _]
           (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                      (= action GLFW/GLFW_PRESS))
             (GLFW/glfwSetWindowShouldClose win true))
           (when (and (= key GLFW/GLFW_KEY_W)
                      (= action GLFW/GLFW_PRESS))
             (let [enabled (swap! wireframe? not)
                   mode (if enabled GL11/GL_LINE GL11/GL_FILL)]
               (GL11/glPolygonMode GL11/GL_FRONT_AND_BACK mode)))
           (when (and (= key GLFW/GLFW_KEY_I)
                      (= action GLFW/GLFW_PRESS))
             (swap! instancing? not))
           (when (and (= key GLFW/GLFW_KEY_F1)
                      (= action GLFW/GLFW_PRESS))
             (swap! show-stats? not))
           (when (#{GLFW/GLFW_PRESS GLFW/GLFW_REPEAT} action)
             (cond
               (= key GLFW/GLFW_KEY_LEFT)
               (swap! view-angles update :yaw
                      #(clamp (- % view-angle-step) view-angle-min view-angle-max))
               (= key GLFW/GLFW_KEY_RIGHT)
               (swap! view-angles update :yaw
                      #(clamp (+ % view-angle-step) view-angle-min view-angle-max))
               (= key GLFW/GLFW_KEY_UP)
               (swap! view-angles update :pitch
                      #(clamp (+ % view-angle-step) view-angle-min view-angle-max))
               (= key GLFW/GLFW_KEY_DOWN)
               (swap! view-angles update :pitch
                      #(clamp (- % view-angle-step) view-angle-min view-angle-max)))))))
      (let [program (create-program (slurp-resource "shaders/instanced.vert")
                                    (slurp-resource "shaders/instanced.frag"))
            time-loc (GL20/glGetUniformLocation program "uTime")
            view-loc (GL20/glGetUniformLocation program "uViewAngles")
            rect-color-loc (GL20/glGetUniformLocation program "uRectColor")
            use-global-color-loc (GL20/glGetUniformLocation program "uUseGlobalColor")
            rot-speed-loc (GL20/glGetUniformLocation program "uRotationSpeed")
            overlay-program (create-program (slurp-resource "shaders/overlay.vert")
                                            (slurp-resource "shaders/overlay.frag"))
            overlay-viewport-loc (GL20/glGetUniformLocation overlay-program "uViewport")
            overlay-text-loc (GL20/glGetUniformLocation overlay-program "uText")
            overlay-color-loc (GL20/glGetUniformLocation overlay-program "uTextColor")
            {:keys [vao vbo]} (create-cube-mesh)
            overlay-quad (create-overlay-quad)
            overlay-vao (:vao overlay-quad)
            overlay-vbo (:vbo overlay-quad)
            overlay-tex (create-overlay-texture overlay-tex-width overlay-tex-height)
            overlay-bytes (byte-array (* overlay-tex-width overlay-tex-height))
            overlay-buffer (BufferUtils/createByteBuffer (* overlay-tex-width overlay-tex-height))
            overlay-vertex-buffer (BufferUtils/createFloatBuffer (* 6 4))
            overlay {:tex overlay-tex
                     :tex-w overlay-tex-width
                     :tex-h overlay-tex-height
                     :bytes overlay-bytes
                     :buffer overlay-buffer
                     :vao overlay-vao
                     :vbo overlay-vbo
                     :vertex-buffer overlay-vertex-buffer}
            instance-vbo (do
                           (GL30/glBindVertexArray vao)
                           (create-instance-buffer num-instances))
            {:keys [base params sizes instance-data]} (init-instance-state num-instances)
            instance-buffer (BufferUtils/createFloatBuffer (* num-instances 4))]
        (GL20/glUseProgram program)
        (GL20/glUseProgram overlay-program)
        (when (<= 0 overlay-text-loc)
          (GL20/glUniform1i overlay-text-loc 0))
        (loop []
          (when (not (GLFW/glfwWindowShouldClose window))
            (let [t (GLFW/glfwGetTime)
                  dt (if (pos? @last-time) (- t @last-time) 0.0)
                  _ (reset! last-time t)
                  frame-ms (* dt 1000.0)
                  {:keys [fps]} (swap! fps-state
                                       (fn [{:keys [acc frames fps]}]
                                         (let [acc (+ acc dt)
                                               frames (inc frames)]
                                           (if (>= acc 0.5)
                                             {:acc 0.0 :frames 0 :fps (/ frames acc)}
                                             {:acc acc :frames frames :fps fps}))))
                  update-start (System/nanoTime)]
              (update-instance-data! base params sizes instance-data t @osc-amplitude
                                     @movement-speed @size-scale)
              (let [update-ms (/ (- (System/nanoTime) update-start) 1000000.0)]
                (.clear instance-buffer)
                (.put instance-buffer instance-data)
                (.flip instance-buffer)
                (let [upload-start (System/nanoTime)]
                  (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER instance-vbo)
                  (GL15/glBufferSubData GL15/GL_ARRAY_BUFFER 0 instance-buffer)
                  (let [upload-ms (/ (- (System/nanoTime) upload-start) 1000000.0)
                        render-start (System/nanoTime)]
                    (let [[r g b a] @background-color]
                      (GL11/glClearColor (float r) (float g) (float b) (float a)))
                    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
                    (GL20/glUseProgram program)
                    (when (<= 0 time-loc)
                      (GL20/glUniform1f time-loc (float t)))
                    (when (<= 0 rot-speed-loc)
                      (GL20/glUniform1f rot-speed-loc (float @rotation-speed)))
                    (when (<= 0 view-loc)
                      (let [{:keys [yaw pitch]} @view-angles]
                        (GL20/glUniform2f view-loc (float yaw) (float pitch))))
                    (let [color @rect-color
                          use-global? (and (vector? color) (= 3 (count color)))]
                      (when (<= 0 use-global-color-loc)
                        (GL20/glUniform1f use-global-color-loc (if use-global? 1.0 0.0)))
                      (when (<= 0 rect-color-loc)
                        (if use-global?
                          (let [[rr gg bb] color]
                            (GL20/glUniform3f rect-color-loc (float rr) (float gg) (float bb)))
                          (GL20/glUniform3f rect-color-loc 0.0 0.0 0.0))))
                    (GL30/glBindVertexArray vao)
                    (if @instancing?
                      (do
                        (GL20/glEnableVertexAttribArray 2)
                        (GL20/glEnableVertexAttribArray 3)
                        (GL33/glVertexAttribDivisor 2 1)
                        (GL33/glVertexAttribDivisor 3 1)
                        (GL31/glDrawArraysInstanced GL11/GL_TRIANGLES 0 36 num-instances))
                      (let [x (aget instance-data 0)
                            y (aget instance-data 1)
                            z (aget instance-data 2)
                            size (aget instance-data 3)]
                        (GL20/glDisableVertexAttribArray 2)
                        (GL20/glDisableVertexAttribArray 3)
                        (GL33/glVertexAttribDivisor 2 0)
                        (GL33/glVertexAttribDivisor 3 0)
                        (GL20/glVertexAttrib3f 2 x y z)
                        (GL20/glVertexAttrib1f 3 size)
                        (GL11/glDrawArrays GL11/GL_TRIANGLES 0 36)))
                    (GL30/glBindVertexArray 0)
                    (let [render-ms (/ (- (System/nanoTime) render-start) 1000000.0)]
                      (when @show-stats?
                        (let [instances (if @instancing? num-instances 1)
                              cubes instances
                              triangles (* cubes 12)
                              vertices (* cubes 36)
                              lines [(str "FPS: " (format-fps fps))
                                     (str "Frame: " (format-ms frame-ms) " ms")
                                     (str "Cubes: " (format-int cubes))
                                     (str "Triangles: " (format-int triangles))
                                     (str "Vertices: " (format-int vertices))
                                     (str "Draw Calls: 1")
                                     (str "Instances: " (format-int instances))
                                     (str "Update: " (format-ms update-ms) " ms")
                                     (str "Upload: " (format-ms upload-ms) " ms")
                                     (str "Render: " (format-ms render-ms) " ms")
                                     "[F1] Toggle Stats"]]
                          (update-overlay! overlay lines)
                          (GL20/glUseProgram overlay-program)
                          (when (<= 0 overlay-viewport-loc)
                            (GL20/glUniform2f overlay-viewport-loc (float @width) (float @height)))
                          (when (<= 0 overlay-color-loc)
                            (GL20/glUniform3f overlay-color-loc 0.2 1.0 0.2))
                          (GL13/glActiveTexture GL13/GL_TEXTURE0)
                          (GL11/glBindTexture GL11/GL_TEXTURE_2D overlay-tex)
                          (GL30/glBindVertexArray overlay-vao)
                          (GL11/glEnable GL11/GL_BLEND)
                          (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
                          (GL11/glDrawArrays GL11/GL_TRIANGLES 0 6)
                          (GL11/glDisable GL11/GL_BLEND)
                          (GL30/glBindVertexArray 0)))))

                  (GLFW/glfwSwapBuffers window)
                  (GLFW/glfwPollEvents)
                  (recur))))))
        (cleanup! window vao vbo instance-vbo program
                  overlay-vao overlay-vbo overlay-tex overlay-program
                  error-callback @gl-ready? @nrepl-server))
      (catch Throwable t
        (cleanup! window 0 0 0 0 0 0 0 0 error-callback @gl-ready? @nrepl-server)
        (throw t)))))

(defn -main
  [& _]
  (run-app!))
