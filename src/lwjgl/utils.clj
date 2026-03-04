(ns lwjgl.utils
  "Java/OpenGL 脚手架工具函数
  提供 GLFW 窗口管理、OpenGL 资源创建、Shader 编译、nREPL 等基础设施"
  (:require [clojure.java.io :as io]
            [nrepl.server :as nrepl])
  (:import (org.lwjgl.glfw GLFW GLFWErrorCallback GLFWFramebufferSizeCallbackI)
           (org.lwjgl.opengl GL GL45 GL46)  ; 使用 GL45/GL46 统一访问所有 OpenGL 函数
           (org.lwjgl BufferUtils)
           (java.nio FloatBuffer ByteBuffer)
           (java.util Arrays)))

;; ---------------------------------------------------------------------------
;; 常量定义

(def ^:const overlay-tex-width 512)
(def ^:const overlay-tex-height 256)
(def ^:const font-width 5)
(def ^:const font-height 7)
(def ^:const font-scale 2)
(def ^:const font-spacing 1)
(def ^:const line-spacing 2)
(def ^:const overlay-margin 12)

;; 立方体顶点数据 (position xyz + normal xyz)
(def cube-vertices
  (float-array
   [;; front
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

;; 字体位图数据 (5x7 像素)
(def ^:private font-glyphs
  {\space ["00000" "00000" "00000" "00000" "00000" "00000" "00000"]
   \.     ["00000" "00000" "00000" "00000" "00000" "00100" "00100"]
   \,     ["00000" "00000" "00000" "00000" "00100" "00100" "01000"]
   \:     ["00000" "00100" "00100" "00000" "00100" "00100" "00000"]
   \[     ["01110" "01000" "01000" "01000" "01000" "01000" "01110"]
   \]     ["01110" "00010" "00010" "00010" "00010" "00010" "01110"]
   \?     ["01110" "10001" "00010" "00100" "00100" "00000" "00100"]
   \0     ["01110" "10001" "10011" "10101" "11001" "10001" "01110"]
   \1     ["00100" "01100" "00100" "00100" "00100" "00100" "01110"]
   \2     ["01110" "10001" "00001" "00010" "00100" "01000" "11111"]
   \3     ["11110" "00001" "00001" "01110" "00001" "00001" "11110"]
   \4     ["00010" "00110" "01010" "10010" "11111" "00010" "00010"]
   \5     ["11111" "10000" "11110" "00001" "00001" "10001" "01110"]
   \6     ["00110" "01000" "10000" "11110" "10001" "10001" "01110"]
   \7     ["11111" "00001" "00010" "00100" "01000" "01000" "01000"]
   \8     ["01110" "10001" "10001" "01110" "10001" "10001" "01110"]
   \9     ["01110" "10001" "10001" "01111" "00001" "00010" "01100"]
   \A     ["01110" "10001" "10001" "11111" "10001" "10001" "10001"]
   \B     ["11110" "10001" "10001" "11110" "10001" "10001" "11110"]
   \C     ["01110" "10001" "10000" "10000" "10000" "10001" "01110"]
   \D     ["11110" "10001" "10001" "10001" "10001" "10001" "11110"]
   \E     ["11111" "10000" "10000" "11110" "10000" "10000" "11111"]
   \F     ["11111" "10000" "10000" "11110" "10000" "10000" "10000"]
   \G     ["01110" "10001" "10000" "10111" "10001" "10001" "01110"]
   \H     ["10001" "10001" "10001" "11111" "10001" "10001" "10001"]
   \I     ["01110" "00100" "00100" "00100" "00100" "00100" "01110"]
   \J     ["00001" "00001" "00001" "00001" "10001" "10001" "01110"]
   \K     ["10001" "10010" "10100" "11000" "10100" "10010" "10001"]
   \L     ["10000" "10000" "10000" "10000" "10000" "10000" "11111"]
   \M     ["10001" "11011" "10101" "10001" "10001" "10001" "10001"]
   \N     ["10001" "11001" "10101" "10011" "10001" "10001" "10001"]
   \O     ["01110" "10001" "10001" "10001" "10001" "10001" "01110"]
   \P     ["11110" "10001" "10001" "11110" "10000" "10000" "10000"]
   \Q     ["01110" "10001" "10001" "10001" "10001" "10011" "01101"]
   \R     ["11110" "10001" "10001" "11110" "10100" "10010" "10001"]
   \S     ["01111" "10000" "10000" "01110" "00001" "00001" "11110"]
   \T     ["11111" "00100" "00100" "00100" "00100" "00100" "00100"]
   \U     ["10001" "10001" "10001" "10001" "10001" "10001" "01110"]
   \V     ["10001" "10001" "10001" "10001" "10001" "01010" "00100"]
   \W     ["10001" "10001" "10001" "10101" "10101" "10101" "01010"]
   \X     ["10001" "10001" "01010" "00100" "01010" "10001" "10001"]
   \Y     ["10001" "10001" "01010" "00100" "00100" "00100" "00100"]
   \Z     ["11111" "00001" "00010" "00100" "01000" "10000" "11111"]
   \-     ["00000" "00000" "00000" "11111" "00000" "00000" "00000"]
   \_     ["00000" "00000" "00000" "00000" "00000" "00000" "11111"]
   \(     ["00010" "00100" "01000" "01000" "01000" "00100" "00010"]
   \)     ["01000" "00100" "00010" "00010" "00010" "00100" "01000"]
   \/     ["00001" "00001" "00010" "00100" "01000" "10000" "10000"]
   \*     ["00000" "00100" "10101" "01110" "10101" "00100" "00000"]
   \=     ["00000" "00000" "11111" "00000" "11111" "00000" "00000"]
   \+     ["00000" "00100" "00100" "11111" "00100" "00100" "00000"]
   \>     ["10000" "01000" "00100" "00010" "00100" "01000" "10000"]
   \<     ["00001" "00010" "00100" "01000" "00100" "00010" "00001"]})

;; ---------------------------------------------------------------------------
;; GLFW 窗口管理

(defn init-glfw!
  "初始化 GLFW 并返回错误回调对象"
  []
  (let [error-callback (GLFWErrorCallback/createPrint System/err)]
    (.set error-callback)
    (when-not (GLFW/glfwInit)
      (throw (IllegalStateException. "Unable to initialize GLFW")))
    error-callback))

(defn create-window
  "创建 GLFW 窗口
  参数:
    width  - 窗口宽度
    height - 窗口高度
    title  - 窗口标题"
  [width height title]
  (GLFW/glfwDefaultWindowHints)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MAJOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_CONTEXT_VERSION_MINOR 3)
  (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_PROFILE GLFW/GLFW_OPENGL_CORE_PROFILE)
  (when (= (System/getProperty "os.name") "Mac OS X")
    (GLFW/glfwWindowHint GLFW/GLFW_OPENGL_FORWARD_COMPAT GL45/GL_TRUE))
  (GLFW/glfwWindowHint GLFW/GLFW_RESIZABLE GL45/GL_TRUE)
  (let [window (GLFW/glfwCreateWindow width height title 0 0)]
    (when (zero? window)
      (throw (RuntimeException. "Failed to create GLFW window")))
    (GLFW/glfwMakeContextCurrent window)
    (GLFW/glfwSwapInterval 1) ;; 垂直同步刷新
    (GLFW/glfwShowWindow window)
    window))

(defn init-viewport!
  "使用 framebuffer 大小初始化 OpenGL viewport
  参数:
    window - GLFW 窗口句柄
    width  - 宽度原子(可选, 会被更新)
    height - 高度原子(可选, 会被更新)"
  [window width height]
  (let [w (BufferUtils/createIntBuffer 1)
        h (BufferUtils/createIntBuffer 1)]
    (GLFW/glfwGetFramebufferSize window w h)
    (let [fw (.get w 0)
          fh (.get h 0)]
      (when (instance? clojure.lang.IAtom width)
        (reset! width fw))
      (when (instance? clojure.lang.IAtom height)
        (reset! height fh))
      (GL45/glViewport 0 0 fw fh))))

;; ---------------------------------------------------------------------------
;; Shader 和 Program

(defn compile-shader
  "编译单个 shader
  参数:
    shader-type - GL_VERTEX_SHADER 或 GL_FRAGMENT_SHADER
    source      - shader 源代码字符串
  返回: shader ID"
  [shader-type source]
  (let [shader (GL45/glCreateShader shader-type)]
    (GL45/glShaderSource shader source)
    (GL45/glCompileShader shader)
    (when (zero? (GL45/glGetShaderi shader GL45/GL_COMPILE_STATUS))
      (let [log (GL45/glGetShaderInfoLog shader)]
        (GL45/glDeleteShader shader)
        (throw (RuntimeException. (str "Shader compile failed: " log)))))
    shader))

(defn create-program
  "创建 shader program
  参数:
    vs-source - 顶点 shader 源代码
    fs-source - 片段 shader 源代码
  返回: program ID"
  [vs-source fs-source]
  (let [vs (compile-shader GL45/GL_VERTEX_SHADER vs-source)
        fs (compile-shader GL45/GL_FRAGMENT_SHADER fs-source)
        program (GL45/glCreateProgram)]
    (GL45/glAttachShader program vs)
    (GL45/glAttachShader program fs)
    (GL45/glLinkProgram program)
    (when (zero? (GL45/glGetProgrami program GL45/GL_LINK_STATUS))
      (let [log (GL45/glGetProgramInfoLog program)]
        (GL45/glDeleteProgram program)
        (throw (RuntimeException. (str "Program link failed: " log)))))
    (GL45/glDeleteShader vs)
    (GL45/glDeleteShader fs)
    program))

;; ---------------------------------------------------------------------------
;; Buffer 工具

(defn create-float-buffer
  "创建指定大小的 FloatBuffer"
  ^FloatBuffer [size]
  (BufferUtils/createFloatBuffer size))

(defn create-byte-buffer
  "创建指定大小的 ByteBuffer"
  ^ByteBuffer [size]
  (BufferUtils/createByteBuffer size))

(defn fill-float-buffer!
  "用 float-array 填充 FloatBuffer"
  [^FloatBuffer buf ^floats arr]
  (.clear buf)
  (.put buf arr)
  (.flip buf)
  buf)

(defn fill-byte-buffer!
  "用 byte-array 填充 ByteBuffer"
  [^ByteBuffer buf ^bytes arr]
  (.clear buf)
  (.put buf arr)
  (.flip buf)
  buf)

;; ---------------------------------------------------------------------------
;; VAO/VBO 创建

(defn create-vao
  "创建并绑定一个新的 VAO, 返回 VAO ID"
  []
  (let [vao (GL45/glGenVertexArrays)]
    (GL45/glBindVertexArray vao)
    vao))

(defn create-vbo
  "创建 VBO 并绑定到 GL_ARRAY_BUFFER
  参数:
    data-or-size - FloatBuffer 或大小(字节)
    usage        - GL_STATIC_DRAW 或 GL_DYNAMIC_DRAW 等
  返回: VBO ID"
  [data-or-size usage]
  (let [vbo (GL45/glGenBuffers)]
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (if (instance? FloatBuffer data-or-size)
      (GL45/glBufferData GL45/GL_ARRAY_BUFFER ^FloatBuffer data-or-size usage)
      (GL45/glBufferData GL45/GL_ARRAY_BUFFER ^long data-or-size usage))
    vbo))

(defn create-vbo-from-array
  "从 float-array 创建静态 VBO"
  [^floats arr]
  (let [vbo (GL45/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength arr))]
    (.put buf arr)
    (.flip buf)
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER buf GL45/GL_STATIC_DRAW)
    vbo))

(defn set-vertex-attrib!
  "设置顶点属性指针
  参数:
    index  - 属性索引
    size   - 每个属性的分量数(1-4)
    stride - 步幅(字节)
    offset - 偏移量(字节)"
  ([index size stride offset]
   (GL45/glVertexAttribPointer index size GL45/GL_FLOAT false stride offset)
   (GL45/glEnableVertexAttribArray index))
  ([index size]
   (set-vertex-attrib! index size 0 0)))

(defn set-instanced-attrib!
  "设置实例化顶点属性
  参数:
    index  - 属性索引
    size   - 每个属性的分量数
    stride - 步幅(字节)
    offset - 偏移量(字节)"
  [index size stride offset]
  (GL45/glVertexAttribPointer index size GL45/GL_FLOAT false stride offset)
  (GL45/glEnableVertexAttribArray index)
  (GL45/glVertexAttribDivisor index 1))

;; ---------------------------------------------------------------------------
;; 纹理创建

(defn create-texture-2d
  "创建 2D 纹理
  参数:
    width       - 纹理宽度
    height      - 纹理高度
    internal-fmt- 内部格式(如 GL_R8)
    format      - 数据格式(如 GL_RED)
    type        - 数据类型(如 GL_UNSIGNED_BYTE)
  返回: 纹理 ID"
  ([width height internal-fmt format type]
   (let [tex (GL45/glGenTextures)]
     (GL45/glBindTexture GL45/GL_TEXTURE_2D tex)
     (GL45/glPixelStorei GL45/GL_UNPACK_ALIGNMENT 1)
     (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_MIN_FILTER GL45/GL_NEAREST)
     (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_MAG_FILTER GL45/GL_NEAREST)
     (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_WRAP_S GL45/GL_CLAMP_TO_EDGE)
     (GL45/glTexParameteri GL45/GL_TEXTURE_2D GL45/GL_TEXTURE_WRAP_T GL45/GL_CLAMP_TO_EDGE)
     (GL45/glTexImage2D GL45/GL_TEXTURE_2D 0 internal-fmt width height 0 format type nil)
     tex))
  ([width height]
   (create-texture-2d width height GL45/GL_RGBA GL45/GL_RGBA GL45/GL_UNSIGNED_BYTE)))

(defn update-texture-2d!
  "更新 2D 纹理数据"
  [tex width height format type ^ByteBuffer data]
  (GL45/glBindTexture GL45/GL_TEXTURE_2D tex)
  (GL45/glTexSubImage2D GL45/GL_TEXTURE_2D 0 0 0 width height format type data))

;; ---------------------------------------------------------------------------
;; 资源加载

(defn slurp-resource
  "从 resources 目录读取文件内容"
  [path]
  (slurp (io/resource path)))

;; ---------------------------------------------------------------------------
;; nREPL

(defn start-nrepl!
  "启动 nREPL 服务器, 返回服务器对象"
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
  "停止 nREPL 服务器"
  [server]
  (when server
    (nrepl/stop-server server)
    (io/delete-file ".nrepl-port" true)))

;; ---------------------------------------------------------------------------
;; 清理工具

(defn delete-vao!
  "删除 VAO"
  [vao]
  (when (and (number? vao) (pos? vao))
    (GL45/glDeleteVertexArrays vao)))

(defn delete-vbo!
  "删除 VBO"
  [vbo]
  (when (and (number? vbo) (pos? vbo))
    (GL45/glDeleteBuffers vbo)))

(defn delete-program!
  "删除 Shader Program"
  [program]
  (when (and (number? program) (pos? program))
    (GL45/glDeleteProgram program)))

(defn delete-texture!
  "删除纹理"
  [tex]
  (when (and (number? tex) (pos? tex))
    (GL45/glDeleteTextures tex)))

(defn cleanup-gl!
  "清理所有 OpenGL 资源"
  [& {:keys [vaos vbos programs textures]}]
  (GL45/glUseProgram 0)
  (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER 0)
  (GL45/glBindVertexArray 0)
  (doseq [t textures] (delete-texture! t))
  (doseq [p programs] (delete-program! p))
  (doseq [v vbos] (delete-vbo! v))
  (doseq [v vaos] (delete-vao! v)))

;; ---------------------------------------------------------------------------
;; 数学工具

(defn clamp
  "将值限制在 [min-val, max-val] 范围内"
  ^double [^double v ^double min-val ^double max-val]
  (-> v (max min-val) (min max-val)))

;; ---------------------------------------------------------------------------
;; 数组工具

(defn clear-byte-array!
  "将 byte-array 清零"
  [^bytes buf]
  (Arrays/fill buf (byte 0)))

(def ^{:doc "清除文本缓冲区 (clear-byte-array! 的别名)"}
  clear-text-buffer!
  clear-byte-array!)

;; ---------------------------------------------------------------------------
;; 字体渲染工具

(defn- glyph-pattern
  "获取字符的位图模式"
  [ch]
  (or (get font-glyphs ch)
      (get font-glyphs (Character/toUpperCase ch))
      (get font-glyphs \?)))

(defn- set-text-pixel!
  "在字节缓冲区中设置一个像素"
  [^bytes buf tex-w tex-h x y]
  (when (and (<= 0 x) (< x tex-w) (<= 0 y) (< y tex-h))
    (aset-byte buf (+ x (* y tex-w)) (byte -1))))

(defn- draw-glyph!
  "在位图缓冲区中绘制一个字符"
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
  "在位图缓冲区中绘制字符串
  参数:
    buf   - byte-array 缓冲区
    tex-w - 纹理宽度
    tex-h - 纹理高度
    x     - 起始 x 坐标
    y     - 起始 y 坐标
    s     - 要绘制的字符串"
  [^bytes buf tex-w tex-h x y ^String s]
  (let [advance (+ (* font-width font-scale) (* font-spacing font-scale))]
    (dotimes [idx (.length s)]
      (let [pattern (glyph-pattern (.charAt s idx))]
        (draw-glyph! buf tex-w tex-h (+ x (* idx advance)) y pattern)))))

(defn text-size
  "计算文本的像素尺寸 (基于字体常量)"
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

;; ---------------------------------------------------------------------------
;; 覆盖层渲染

(defn create-overlay-texture
  "创建 UI 覆盖层纹理 (单通道 R8 格式)"
  [w h]
  (create-texture-2d w h GL45/GL_R8 GL45/GL_RED GL45/GL_UNSIGNED_BYTE))

(defn create-overlay-quad
  "创建 UI 覆盖层四边形 VAO/VBO
  返回包含 :vao 和 :vbo 的 map"
  []
  (let [vao (create-vao)
        vbo (GL45/glGenBuffers)
        stride (* 4 Float/BYTES)]
    (GL45/glBindBuffer GL45/GL_ARRAY_BUFFER vbo)
    (GL45/glBufferData GL45/GL_ARRAY_BUFFER (* 6 4 Float/BYTES) GL45/GL_STREAM_DRAW)
    (set-vertex-attrib! 0 2 stride 0)           ; position
    (set-vertex-attrib! 1 2 stride (* 2 Float/BYTES)) ; uv
    (GL45/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn create-cube-mesh
  "创建立方体网格 VAO/VBO
  返回包含 :vao 和 :vbo 的 map"
  []
  (let [vao (create-vao)
        vbo (create-vbo-from-array cube-vertices)
        stride (* 6 Float/BYTES)]
    (set-vertex-attrib! 0 3 stride 0)           ; position
    (set-vertex-attrib! 1 3 stride (* 3 Float/BYTES)) ; normal
    (GL45/glBindVertexArray 0)
    {:vao vao :vbo vbo}))
