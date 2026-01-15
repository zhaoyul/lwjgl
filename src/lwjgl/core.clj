(ns lwjgl.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [nrepl.server :as nrepl])
  (:import (org.lwjgl.glfw GLFW GLFWErrorCallback GLFWFramebufferSizeCallbackI
                            GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30 GL31 GL33)
           (org.lwjgl BufferUtils)
           (java.util Random)))

(def ^:const num-instances 10000)
(def ^:const tau (* 2.0 Math/PI))
(defonce osc-amplitude (atom 0.08))
(defonce rect-color (atom [0.2 0.6 0.9]))
(defonce background-color (atom [0.06 0.06 0.08 1.0]))
(defonce rotation-speed (atom 1.0))
(defonce movement-speed (atom 1.0))
(defonce size-scale (atom 1.0))
(defonce wireframe? (atom false))
(defonce instancing? (atom true))
(defonce view-angles (atom {:yaw 0.0 :pitch 0.0}))
(def ^:const view-angle-min -5.0)
(def ^:const view-angle-max 5.0)
(def ^:const view-angle-step 1.0)

(def rect-vertices
  (float-array
   [;; two triangles forming a unit rectangle centered at the origin
    -0.5 -0.5
     0.5 -0.5
     0.5  0.5
     0.5  0.5
    -0.5  0.5
    -0.5 -0.5]))

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
        base (float-array (* num 2))
        params (float-array (* num 4))
        sizes (float-array (* num 2))
        instance-data (float-array (* num 4))]
    (loop [i 0
           b 0
           p 0
           s 0]
      (when (< i num)
        (let [x (rand-range rng -0.9 0.9)
              y (rand-range rng -0.9 0.9)
              w (rand-range rng 0.01 0.05)
              h (rand-range rng 0.01 0.05)
              fx (rand-range rng 0.4 1.4)
              fy (rand-range rng 0.4 1.4)
              phx (rand-range rng 0.0 tau)
              phy (rand-range rng 0.0 tau)]
          (aset base b (float x))
          (aset base (unchecked-add-int b 1) (float y))
          (aset sizes s (float w))
          (aset sizes (unchecked-add-int s 1) (float h))
          (aset params p (float fx))
          (aset params (unchecked-add-int p 1) (float fy))
          (aset params (unchecked-add-int p 2) (float phx))
          (aset params (unchecked-add-int p 3) (float phy)))
        (recur (unchecked-inc-int i)
               (unchecked-add-int b 2)
               (unchecked-add-int p 4)
               (unchecked-add-int s 2))))
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
        num (quot (alength base) 2)]
    (loop [i 0
           b 0
           p 0
           s 0
           inst 0]
      (when (< i num)
        (let [bx (aget base b)
              by (aget base (unchecked-add-int b 1))
              fx (aget params p)
              fy (aget params (unchecked-add-int p 1))
              phx (aget params (unchecked-add-int p 2))
              phy (aget params (unchecked-add-int p 3))
              w (aget sizes s)
              h (aget sizes (unchecked-add-int s 1))
              x (+ bx (* osc-amplitude (Math/sin (+ (* fx t movement-speed) phx))))
              y (+ by (* osc-amplitude (Math/sin (+ (* fy t movement-speed) phy))))]
          (aset instance-data inst (float x))
          (aset instance-data (unchecked-add-int inst 1) (float y))
          (aset instance-data (unchecked-add-int inst 2) (float (* w size-scale)))
          (aset instance-data (unchecked-add-int inst 3) (float (* h size-scale)))
          (recur (unchecked-inc-int i)
                 (unchecked-add-int b 2)
                 (unchecked-add-int p 4)
                 (unchecked-add-int s 2)
                 (unchecked-add-int inst 4)))))))

(defn slurp-resource
  [path]
  (slurp (io/resource path)))

(defn start-nrepl!
  []
  (let [server (nrepl/start-server :port 0)
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

(defn create-rectangle-mesh
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength rect-vertices))]
    (GL30/glBindVertexArray vao)
    (.put buf rect-vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false (* 2 Float/BYTES) 0)
    (GL20/glEnableVertexAttribArray 0)
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
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 1)
    (GL20/glVertexAttribPointer 2 2 GL11/GL_FLOAT false stride (* 2 Float/BYTES))
    (GL20/glEnableVertexAttribArray 2)
    (GL33/glVertexAttribDivisor 1 1)
    (GL33/glVertexAttribDivisor 2 1)
    vbo))

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
  [window vao vbo instance-vbo program error-callback gl-ready? nrepl-server]
  (when gl-ready?
    (GL20/glUseProgram 0)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER 0)
    (GL30/glBindVertexArray 0)
    (when (pos? vbo) (GL15/glDeleteBuffers vbo))
    (when (pos? instance-vbo) (GL15/glDeleteBuffers instance-vbo))
    (when (pos? vao) (GL30/glDeleteVertexArrays vao))
    (when (pos? program) (GL20/glDeleteProgram program)))
  (stop-nrepl! nrepl-server)
  (when (pos? window) (GLFW/glfwDestroyWindow window))
  (GLFW/glfwTerminate)
  (.free error-callback))

(defn run-app!
  []
  (let [error-callback (init-glfw!)
        width (atom 1200)
        height (atom 800)
        window (create-window @width @height "LWJGL Rectangles")
        gl-ready? (atom false)
        nrepl-server (atom nil)]
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
            rot-speed-loc (GL20/glGetUniformLocation program "uRotationSpeed")
            {:keys [vao vbo]} (create-rectangle-mesh)
            instance-vbo (do
                           (GL30/glBindVertexArray vao)
                           (create-instance-buffer num-instances))
            {:keys [base params sizes instance-data]} (init-instance-state num-instances)
            instance-buffer (BufferUtils/createFloatBuffer (* num-instances 4))]
        (GL20/glUseProgram program)
        (loop []
          (when (not (GLFW/glfwWindowShouldClose window))
            (let [t (GLFW/glfwGetTime)]
              (update-instance-data! base params sizes instance-data t @osc-amplitude
                                     @movement-speed @size-scale)
              (.clear instance-buffer)
              (.put instance-buffer instance-data)
              (.flip instance-buffer)
              (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER instance-vbo)
              (GL15/glBufferSubData GL15/GL_ARRAY_BUFFER 0 instance-buffer)

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
              (when (<= 0 rect-color-loc)
                (let [[r g b] @rect-color]
                  (GL20/glUniform3f rect-color-loc (float r) (float g) (float b))))
              (GL30/glBindVertexArray vao)
              (if @instancing?
                (do
                  (GL20/glEnableVertexAttribArray 1)
                  (GL20/glEnableVertexAttribArray 2)
                  (GL33/glVertexAttribDivisor 1 1)
                  (GL33/glVertexAttribDivisor 2 1)
                  (GL31/glDrawArraysInstanced GL11/GL_TRIANGLES 0 6 num-instances))
                (let [x (aget instance-data 0)
                      y (aget instance-data 1)
                      w (aget instance-data 2)
                      h (aget instance-data 3)]
                  (GL20/glDisableVertexAttribArray 1)
                  (GL20/glDisableVertexAttribArray 2)
                  (GL33/glVertexAttribDivisor 1 0)
                  (GL33/glVertexAttribDivisor 2 0)
                  (GL20/glVertexAttrib2f 1 x y)
                  (GL20/glVertexAttrib2f 2 w h)
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 6)))
              (GL30/glBindVertexArray 0)

              (GLFW/glfwSwapBuffers window)
              (GLFW/glfwPollEvents)
              (recur))))
        (cleanup! window vao vbo instance-vbo program error-callback @gl-ready? @nrepl-server))
      (catch Throwable t
        (cleanup! window 0 0 0 0 error-callback @gl-ready? @nrepl-server)
        (throw t)))))

(defn -main
  [& _]
  (run-app!))
