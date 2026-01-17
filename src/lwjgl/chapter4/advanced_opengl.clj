(ns lwjgl.chapter4.advanced-opengl
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWFramebufferSizeCallbackI GLFWKeyCallbackI)
           (org.lwjgl.opengl GL GL11 GL12 GL13 GL15 GL20 GL30 GL31 GL32 GL33)
           (org.joml Matrix4f Vector3f)))

(def ^:const width 800)
(def ^:const height 600)

(defn- upload-mat!
  [^Matrix4f m ^java.nio.FloatBuffer buf loc]
  (.clear buf)
  (.get m buf)
  (.flip buf)
  (when (<= 0 loc)
    (GL20/glUniformMatrix4fv loc false buf)))

(defn- create-program*
  ([vs-src fs-src]
   (core/create-program vs-src fs-src))
  ([vs-src fs-src geom-src]
   (let [vs (core/compile-shader GL20/GL_VERTEX_SHADER vs-src)
         gs (core/compile-shader GL32/GL_GEOMETRY_SHADER geom-src)
         fs (core/compile-shader GL20/GL_FRAGMENT_SHADER fs-src)
         program (GL20/glCreateProgram)]
     (GL20/glAttachShader program vs)
     (GL20/glAttachShader program gs)
     (GL20/glAttachShader program fs)
     (GL20/glLinkProgram program)
     (when (zero? (GL20/glGetProgrami program GL20/GL_LINK_STATUS))
       (let [log (GL20/glGetProgramInfoLog program)]
         (GL20/glDeleteProgram program)
         (throw (RuntimeException. (str "Program link failed: " log)))))
     (GL20/glDeleteShader vs)
     (GL20/glDeleteShader gs)
     (GL20/glDeleteShader fs)
     program)))

(defn- create-cube-mesh
  []
  (let [vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength core/cube-vertices))
        stride (* 6 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf core/cube-vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 3 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count 36}))

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
        stride (* 4 Float/BYTES)]
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

(defn- create-plane
  []
  (let [vertices (float-array
                  [;; positions     ;; normals    ;; tex
                   -5.0 0.0 -5.0    0.0 1.0 0.0    0.0 0.0
                   5.0 0.0 -5.0     0.0 1.0 0.0    5.0 0.0
                   5.0 0.0 5.0      0.0 1.0 0.0    5.0 5.0
                   5.0 0.0 5.0      0.0 1.0 0.0    5.0 5.0
                   -5.0 0.0 5.0     0.0 1.0 0.0    0.0 5.0
                   -5.0 0.0 -5.0    0.0 1.0 0.0    0.0 0.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 8 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
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
    {:vao vao :vbo vbo :count 6}))

(defn- create-cubemap
  []
  (let [tex (GL11/glGenTextures)
        face-colors [[255 80 80] [80 255 80] [80 80 255] [255 255 80] [80 255 255] [255 80 255]]
        buf (BufferUtils/createByteBuffer (* 1 1 3))]
    (GL11/glBindTexture GL13/GL_TEXTURE_CUBE_MAP tex)
    (doseq [[idx [r g b]] (map-indexed vector face-colors)]
      (.clear buf)
      (.put buf (byte r))
      (.put buf (byte g))
      (.put buf (byte b))
      (.flip buf)
      (GL11/glTexImage2D (+ GL13/GL_TEXTURE_CUBE_MAP_POSITIVE_X idx)
                         0 GL11/GL_RGB 1 1 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE buf))
    (GL11/glTexParameteri GL13/GL_TEXTURE_CUBE_MAP GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL13/GL_TEXTURE_CUBE_MAP GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL13/GL_TEXTURE_CUBE_MAP GL11/GL_TEXTURE_WRAP_S GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexParameteri GL13/GL_TEXTURE_CUBE_MAP GL11/GL_TEXTURE_WRAP_T GL12/GL_CLAMP_TO_EDGE)
    (GL11/glTexParameteri GL13/GL_TEXTURE_CUBE_MAP GL13/GL_TEXTURE_WRAP_R GL12/GL_CLAMP_TO_EDGE)
    tex))

(defn- skybox-program
  []
  (create-program*
   "#version 330 core
layout (location = 0) in vec3 aPos;
out vec3 TexCoords;
uniform mat4 view;
uniform mat4 projection;
void main() {
    TexCoords = aPos;
    vec4 pos = projection * view * vec4(aPos, 1.0);
    gl_Position = pos.xyww;
}"
   "#version 330 core
out vec4 FragColor;
in vec3 TexCoords;
uniform samplerCube skybox;
void main() {
    FragColor = texture(skybox, TexCoords);
}"))

(defn- reflective-program
  []
  (create-program*
   "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
out vec3 WorldPos;
out vec3 Normal;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
    WorldPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    gl_Position = projection * view * vec4(WorldPos, 1.0);
}"
   "#version 330 core
in vec3 WorldPos;
in vec3 Normal;
out vec4 FragColor;
uniform vec3 cameraPos;
uniform samplerCube skybox;
void main() {
    vec3 I = normalize(WorldPos - cameraPos);
    vec3 R = reflect(I, normalize(Normal));
    FragColor = texture(skybox, R);
}"))

(defn- simple-program
  []
  (create-program*
   "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
out vec3 Normal;
out vec3 FragPos;
void main() {
    FragPos = vec3(model * vec4(aPos, 1.0));
    Normal = mat3(transpose(inverse(model))) * aNormal;
    gl_Position = projection * view * vec4(aPos, 1.0);
}"
   "#version 330 core
in vec3 Normal;
in vec3 FragPos;
out vec4 FragColor;
uniform vec3 baseColor;
void main() {
    float diff = max(dot(normalize(Normal), normalize(vec3(0.4, 1.0, 0.3))), 0.0);
    FragColor = vec4(baseColor * (0.2 + 0.8 * diff), 1.0);
}"))

(defn- discard-program
  []
  (create-program*
   "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec2 aTex;
out vec2 TexCoord;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;
void main() {
    TexCoord = aTex;
    gl_Position = projection * view * vec4(aPos, 1.0);
}"
   "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform vec4 baseColor;
void main() {
    float alpha = 1.0 - smoothstep(0.3, 0.5, length(TexCoord - vec2(0.5)));
    if (alpha < 0.1) discard;
    FragColor = vec4(baseColor.rgb, alpha * baseColor.a);
}"))

(defn- discard-quad
  []
  (let [vertices (float-array
                  [;; pos          ;; tex
                   -0.5 0.0 -0.6    0.0 0.0
                   0.5 0.0 -0.6     1.0 0.0
                   0.5 1.0 -0.6     1.0 1.0
                   0.5 1.0 -0.6     1.0 1.0
                   -0.5 1.0 -0.6    0.0 1.0
                   -0.5 0.0 -0.6    0.0 0.0])
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
    {:vao vao :vbo vbo :count 6}))

(defn- points-mesh
  []
  (let [vertices (float-array
                  [ -1.0 0.0 -1.0
                    -0.2 0.0 0.5
                    0.4 0.0 -0.2
                    0.9 0.0 0.7])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 3 GL11/GL_FLOAT false 0 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo :count 4}))

(defn- instanced-quad-mesh
  []
  (let [quad (float-array
              [ -0.5 -0.5 0.0 0.0
                0.5 -0.5 1.0 0.0
                0.5  0.5 1.0 1.0
                -0.5  0.5 0.0 1.0])
        indices (int-array [0 1 2 2 3 0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        ebo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength quad))
        ibuf (BufferUtils/createIntBuffer (alength indices))]
    (GL30/glBindVertexArray vao)
    (.put buf quad)
    (.flip buf)
    (.put ibuf indices)
    (.flip ibuf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL15/glBindBuffer GL15/GL_ELEMENT_ARRAY_BUFFER ebo)
    (GL15/glBufferData GL15/GL_ELEMENT_ARRAY_BUFFER ibuf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false (* 4 Float/BYTES) 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 2 GL11/GL_FLOAT false (* 4 Float/BYTES) (* 2 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    {:vao vao :vbo vbo :ebo ebo :count 6}))

(defn- create-instance-offsets
  [vao positions]
  (let [data (float-array (mapcat (fn [[x y z]] [x y z]) positions))
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength data))]
    (.put buf data)
    (.flip buf)
    (GL30/glBindVertexArray vao)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 2 3 GL11/GL_FLOAT false 0 0)
    (GL20/glEnableVertexAttribArray 2)
    (GL33/glVertexAttribDivisor 2 1)
    vbo))

(defn- basic-view
  []
  (doto (Matrix4f.)
    (.translate (Vector3f. 0.0 0.0 -3.0))))

(defn- basic-projection
  []
  (doto (Matrix4f.)
    (.perspective (float (Math/toRadians 45.0)) (/ width (float height)) 0.1 100.0)))

(defn- setup-framebuffer
  []
  (let [fbo (GL30/glGenFramebuffers)
        tex (GL11/glGenTextures)
        rbo (GL30/glGenRenderbuffers)]
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER fbo)
    (GL11/glBindTexture GL11/GL_TEXTURE_2D tex)
    (GL11/glTexImage2D GL11/GL_TEXTURE_2D 0 GL11/GL_RGB width height 0 GL11/GL_RGB GL11/GL_UNSIGNED_BYTE nil)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MIN_FILTER GL11/GL_LINEAR)
    (GL11/glTexParameteri GL11/GL_TEXTURE_2D GL11/GL_TEXTURE_MAG_FILTER GL11/GL_LINEAR)
    (GL30/glFramebufferTexture2D GL30/GL_FRAMEBUFFER GL30/GL_COLOR_ATTACHMENT0 GL11/GL_TEXTURE_2D tex 0)
    (GL30/glBindRenderbuffer GL30/GL_RENDERBUFFER rbo)
    (GL30/glRenderbufferStorage GL30/GL_RENDERBUFFER GL30/GL_DEPTH24_STENCIL8 width height)
    (GL30/glFramebufferRenderbuffer GL30/GL_FRAMEBUFFER GL30/GL_DEPTH_STENCIL_ATTACHMENT GL30/GL_RENDERBUFFER rbo)
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
    {:fbo fbo :tex tex :rbo rbo}))

(defn- setup-msaa-framebuffer
  []
  (let [fbo (GL30/glGenFramebuffers)
        rbo (GL30/glGenRenderbuffers)
        depth-rbo (GL30/glGenRenderbuffers)]
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER fbo)
    (GL30/glBindRenderbuffer GL30/GL_RENDERBUFFER rbo)
    (GL30/glRenderbufferStorageMultisample GL30/GL_RENDERBUFFER 4 GL11/GL_RGB width height)
    (GL30/glFramebufferRenderbuffer GL30/GL_FRAMEBUFFER GL30/GL_COLOR_ATTACHMENT0 GL30/GL_RENDERBUFFER rbo)
    (GL30/glBindRenderbuffer GL30/GL_RENDERBUFFER depth-rbo)
    (GL30/glRenderbufferStorageMultisample GL30/GL_RENDERBUFFER 4 GL30/GL_DEPTH24_STENCIL8 width height)
    (GL30/glFramebufferRenderbuffer GL30/GL_FRAMEBUFFER GL30/GL_DEPTH_STENCIL_ATTACHMENT GL30/GL_RENDERBUFFER depth-rbo)
    (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
    {:fbo fbo :rbo rbo :depth depth-rbo}))

(defn- setup-window
  [title]
  (let [error-callback (core/init-glfw!)
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
    {:error-callback error-callback :window window}))

(defn- cleanup-window
  [{:keys [window error-callback]}]
  (when (pos? window) (GLFW/glfwDestroyWindow window))
  (GLFW/glfwTerminate)
  (when error-callback (.free error-callback)))

(defn run-scene!
  [scenario]
  (let [{:keys [window] :as env} (setup-window (str "LearnOpenGL - " (name scenario)))]
    (try
      (let [cube (create-cube-mesh)
            plane (create-plane)
            quad (create-quad)
            discard-quad (discard-quad)
            points (points-mesh)
            mat-buf (BufferUtils/createFloatBuffer 16)
            projection (basic-projection)]
        (GL11/glViewport 0 0 width height)
        (case scenario
          (:depth-testing :depth-testing-view)
          (let [program (simple-program)
                model (Matrix4f.)
                view (basic-view)
                model-loc (GL20/glGetUniformLocation program "model")
                view-loc (GL20/glGetUniformLocation program "view")
                proj-loc (GL20/glGetUniformLocation program "projection")
                color-loc (GL20/glGetUniformLocation program "baseColor")]
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (GL20/glUseProgram program)
            (upload-mat! projection mat-buf proj-loc)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.08 0.08 0.1 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (.identity view)
                (.translate view (Vector3f. 0.0 0.0 -3.5))
                (when (= scenario :depth-testing-view)
                  (.rotate view (float (* 0.25 (GLFW/glfwGetTime))) 0.0 1.0 0.0))
                (upload-mat! view mat-buf view-loc)
                (GL30/glBindVertexArray (:vao cube))
                (doseq [[offset color] [[[-0.7 0.0 -1.0] [0.8 0.2 0.2]]
                                        [[0.6 0.0 -2.0] [0.2 0.8 0.6]]]]
                  (.identity model)
                  (.translate model (Vector3f. (float (first offset))
                                               (float (second offset))
                                               (float (nth offset 2))))
                  (.rotate model (float (GLFW/glfwGetTime)) 0.4 0.6 0.1)
                  (upload-mat! model mat-buf model-loc)
                  (when (<= 0 color-loc)
                    (apply GL20/glUniform3f color-loc color))
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          :stencil-testing
          (let [program (simple-program)
                model (Matrix4f.)
                view (basic-view)
                model-loc (GL20/glGetUniformLocation program "model")
                view-loc (GL20/glGetUniformLocation program "view")
                proj-loc (GL20/glGetUniformLocation program "projection")
                color-loc (GL20/glGetUniformLocation program "baseColor")]
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (GL11/glEnable GL11/GL_STENCIL_TEST)
            (GL20/glUseProgram program)
            (upload-mat! projection mat-buf proj-loc)
            (upload-mat! view mat-buf view-loc)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.07 0.07 0.09 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT GL11/GL_STENCIL_BUFFER_BIT))
                (GL11/glStencilFunc GL11/GL_ALWAYS 1 0xFF)
                (GL11/glStencilOp GL11/GL_KEEP GL11/GL_KEEP GL11/GL_REPLACE)
                (GL11/glStencilMask 0xFF)
                (GL20/glUniform3f color-loc 0.3 0.3 0.35)
                (GL30/glBindVertexArray (:vao plane))
                (.identity model)
                (upload-mat! model mat-buf model-loc)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count plane))
                (GL11/glStencilFunc GL11/GL_ALWAYS 1 0xFF)
                (GL11/glStencilMask 0xFF)
                (GL20/glUniform3f color-loc 0.8 0.4 0.2)
                (GL30/glBindVertexArray (:vao cube))
                (.identity model)
                (.translate model (Vector3f. 0.0 0.5 -1.0))
                (.scale model 0.8)
                (upload-mat! model mat-buf model-loc)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL11/glStencilFunc GL11/GL_NOTEQUAL 1 0xFF)
                (GL11/glStencilMask 0x00)
                (GL11/glDisable GL11/GL_DEPTH_TEST)
                (.identity model)
                (.translate model (Vector3f. 0.0 0.5 -1.0))
                (.scale model 1.05)
                (upload-mat! model mat-buf model-loc)
                (GL20/glUniform3f color-loc 1.0 1.0 1.0)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL11/glStencilMask 0xFF)
                (GL11/glEnable GL11/GL_DEPTH_TEST)
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          (:blending-discard :blending-sort)
          (let [program (discard-program)
                model (Matrix4f.)
                view (basic-view)
                model-loc (GL20/glGetUniformLocation program "model")
                view-loc (GL20/glGetUniformLocation program "view")
                proj-loc (GL20/glGetUniformLocation program "projection")
                color-loc (GL20/glGetUniformLocation program "baseColor")
                quads [[[-1.0 0.0 -0.6] [0.8 0.5 0.2 0.6]]
                       [[0.0 -0.2 -0.3] [0.2 0.6 0.8 0.5]]
                       [[1.0 0.1 -0.8] [0.6 0.8 0.2 0.7]]]]
            (GL11/glEnable GL11/GL_BLEND)
            (GL11/glBlendFunc GL11/GL_SRC_ALPHA GL11/GL_ONE_MINUS_SRC_ALPHA)
            (GL20/glUseProgram program)
            (upload-mat! projection mat-buf proj-loc)
            (upload-mat! view mat-buf view-loc)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.06 0.06 0.08 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL30/glBindVertexArray (:vao discard-quad))
                (doseq [[pos color] (let [list quads]
                                      (if (= scenario :blending-sort)
                                        (sort-by #(-> % first (nth 2)) > list)
                                        list))]
                  (.identity model)
                  (.translate model (Vector3f. (float (first pos))
                                               (float (second pos))
                                               (float (nth pos 2))))
                  (.rotate model (float (* 0.2 (GLFW/glfwGetTime))) 0.0 0.0 1.0)
                  (upload-mat! model mat-buf model-loc)
                  (apply GL20/glUniform4f color-loc color)
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count discard-quad)))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          :face-culling-exercise1
          (let [program (simple-program)
                model (Matrix4f.)
                view (basic-view)
                model-loc (GL20/glGetUniformLocation program "model")
                view-loc (GL20/glGetUniformLocation program "view")
                proj-loc (GL20/glGetUniformLocation program "projection")
                color-loc (GL20/glGetUniformLocation program "baseColor")]
            (GL11/glEnable GL11/GL_CULL_FACE)
            (GL11/glCullFace GL11/GL_BACK)
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (GL20/glUseProgram program)
            (upload-mat! projection mat-buf proj-loc)
            (upload-mat! view mat-buf view-loc)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.04 0.04 0.06 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL30/glBindVertexArray (:vao cube))
                (.identity model)
                (.rotate model (float (GLFW/glfwGetTime)) 0.3 0.5 0.2)
                (upload-mat! model mat-buf model-loc)
                (GL20/glUniform3f color-loc 0.6 0.8 0.3)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL11/glCullFace GL11/GL_FRONT)
                (GL20/glUniform3f color-loc 0.9 0.2 0.2)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL11/glCullFace GL11/GL_BACK)
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          (:framebuffers :framebuffers-exercise1)
          (let [program (simple-program)
                screen-program (create-program*
                                "#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTex;
out vec2 TexCoord;
void main() {
    TexCoord = aTex;
    gl_Position = vec4(aPos, 0.0, 1.0);
}"
                                (if (= scenario :framebuffers-exercise1)
                                  "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform sampler2D screenTex;
void main() {
    vec3 col = texture(screenTex, TexCoord).rgb;
    FragColor = vec4(vec3(1.0) - col, 1.0);
}"
                                  "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
uniform sampler2D screenTex;
void main() {
    FragColor = texture(screenTex, TexCoord);
}"))
                fb (setup-framebuffer)
                model (Matrix4f.)
                view (basic-view)
                model-loc (GL20/glGetUniformLocation program "model")
                view-loc (GL20/glGetUniformLocation program "view")
                proj-loc (GL20/glGetUniformLocation program "projection")
                color-loc (GL20/glGetUniformLocation program "baseColor")]
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER (:fbo fb))
                (GL11/glEnable GL11/GL_DEPTH_TEST)
                (GL11/glClearColor 0.1 0.1 0.12 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL20/glUseProgram program)
                (upload-mat! projection mat-buf proj-loc)
                (upload-mat! view mat-buf view-loc)
                (GL30/glBindVertexArray (:vao cube))
                (.identity model)
                (.translate model (Vector3f. -0.6 0.0 -1.2))
                (.rotate model (float (GLFW/glfwGetTime)) 0.3 0.5 0.2)
                (upload-mat! model mat-buf model-loc)
                (GL20/glUniform3f color-loc 0.8 0.6 0.2)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (.identity model)
                (.translate model (Vector3f. 1.0 0.0 -1.8))
                (.rotate model (float (* 0.7 (GLFW/glfwGetTime))) 0.4 0.2 0.6)
                (upload-mat! model mat-buf model-loc)
                (GL20/glUniform3f color-loc 0.2 0.6 0.9)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
                (GL11/glDisable GL11/GL_DEPTH_TEST)
                (GL11/glClearColor 0.0 0.0 0.0 1.0)
                (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
                (GL20/glUseProgram screen-program)
                (GL13/glActiveTexture GL13/GL_TEXTURE0)
                (GL11/glBindTexture GL11/GL_TEXTURE_2D (:tex fb))
                (GL30/glBindVertexArray (:vao quad))
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count quad))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program)
            (GL20/glDeleteProgram screen-program)
            (GL11/glDeleteTextures (:tex fb))
            (GL30/glDeleteFramebuffers (:fbo fb))
            (GL30/glDeleteRenderbuffers (:rbo fb)))

          (:cubemaps-skybox :cubemaps-environment-mapping)
          (let [cube-map (create-cubemap)
                skybox-program (skybox-program)
                reflective (reflective-program)
                skybox-vao (:vao cube)
                skybox-view (doto (Matrix4f.) (.translate (Vector3f. 0.0 0.0 -3.0)))
                model (Matrix4f.)
                view (basic-view)
                proj (basic-projection)
                skybox-view-loc (GL20/glGetUniformLocation skybox-program "view")
                skybox-proj-loc (GL20/glGetUniformLocation skybox-program "projection")
                refl-model-loc (GL20/glGetUniformLocation reflective "model")
                refl-view-loc (GL20/glGetUniformLocation reflective "view")
                refl-proj-loc (GL20/glGetUniformLocation reflective "projection")
                camera-loc (GL20/glGetUniformLocation reflective "cameraPos")]
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.02 0.02 0.03 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL20/glUseProgram reflective)
                (.identity view)
                (.translate view (Vector3f. 0.0 0.0 -3.0))
                (.rotate view (float (* 0.2 (GLFW/glfwGetTime))) 0.0 1.0 0.0)
                (.identity model)
                (.rotate model (float (* 0.5 (GLFW/glfwGetTime))) 0.3 0.5 0.2)
                (upload-mat! model mat-buf refl-model-loc)
                (upload-mat! view mat-buf refl-view-loc)
                (upload-mat! proj mat-buf refl-proj-loc)
                (GL20/glUniform3f camera-loc 0.0 0.0 0.0)
                (GL13/glActiveTexture GL13/GL_TEXTURE0)
                (GL11/glBindTexture GL13/GL_TEXTURE_CUBE_MAP cube-map)
                (GL30/glBindVertexArray (:vao cube))
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL20/glUseProgram skybox-program)
                (.identity skybox-view)
                (.translate skybox-view (Vector3f. 0.0 0.0 -3.0))
                (upload-mat! skybox-view mat-buf skybox-view-loc)
                (upload-mat! proj mat-buf skybox-proj-loc)
                (GL11/glDepthFunc GL11/GL_LEQUAL)
                (GL30/glBindVertexArray skybox-vao)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GL11/glDepthFunc GL11/GL_LESS)
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram reflective)
            (GL20/glDeleteProgram skybox-program)
            (GL11/glDeleteTextures cube-map))

          :advanced-glsl-ubo
          (let [vs "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (std140) uniform Matrices {
    mat4 view;
    mat4 projection;
};
uniform mat4 model;
out vec3 Normal;
void main() {
    Normal = aNormal;
    gl_Position = projection * view * model * vec4(aPos, 1.0);
}"
                fs "#version 330 core
in vec3 Normal;
out vec4 FragColor;
uniform vec3 color;
void main() {
    float l = max(dot(normalize(Normal), normalize(vec3(0.2,1.0,0.3))), 0.0);
    FragColor = vec4(color * (0.3 + 0.7 * l), 1.0);
}"
                program-a (create-program* vs fs)
                program-b (create-program* vs fs)
                model (Matrix4f.)
                view (basic-view)
                proj (basic-projection)
                ubo (GL15/glGenBuffers)
                view-proj-buf (BufferUtils/createFloatBuffer 32)]
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (GL15/glBindBuffer GL31/GL_UNIFORM_BUFFER ubo)
            (GL15/glBufferData GL31/GL_UNIFORM_BUFFER (* 32 Float/BYTES) GL15/GL_DYNAMIC_DRAW)
            (GL30/glBindBufferBase GL31/GL_UNIFORM_BUFFER 0 ubo)
            (doseq [p [program-a program-b]]
              (let [idx (GL31/glGetUniformBlockIndex p "Matrices")]
                (GL31/glUniformBlockBinding p idx 0)))
            (.clear view-proj-buf)
            (.get view view-proj-buf)
            (.get proj view-proj-buf)
            (.flip view-proj-buf)
            (GL15/glBufferSubData GL31/GL_UNIFORM_BUFFER 0 view-proj-buf)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.05 0.05 0.08 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL30/glBindVertexArray (:vao cube))
                (GL20/glUseProgram program-a)
                (let [model-loc (GL20/glGetUniformLocation program-a "model")
                      color-loc (GL20/glGetUniformLocation program-a "color")]
                  (.identity model)
                  (.translate model (Vector3f. -1.0 0.0 -1.5))
                  (.rotate model (float (GLFW/glfwGetTime)) 0.3 0.5 0.2)
                  (upload-mat! model mat-buf model-loc)
                  (GL20/glUniform3f color-loc 0.9 0.4 0.2)
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube)))
                (GL20/glUseProgram program-b)
                (let [model-loc (GL20/glGetUniformLocation program-b "model")
                      color-loc (GL20/glGetUniformLocation program-b "color")]
                  (.identity model)
                  (.translate model (Vector3f. 1.0 0.0 -1.5))
                  (.rotate model (float (* 0.6 (GLFW/glfwGetTime))) 0.2 0.8 0.1)
                  (upload-mat! model mat-buf model-loc)
                  (GL20/glUniform3f color-loc 0.2 0.6 0.9)
                  (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube)))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program-a)
            (GL20/glDeleteProgram program-b)
            (GL15/glDeleteBuffers ubo))

          :geometry-shader-houses
          (let [program (create-program*
                         "#version 330 core
layout (location = 0) in vec3 aPos;
void main() { gl_Position = vec4(aPos, 1.0); }"
                         "#version 330 core
in vec3 gColor;
out vec4 FragColor;
void main() { FragColor = vec4(gColor, 1.0); }"
                         "#version 330 core
layout (points) in;
layout (triangle_strip, max_vertices = 5) out;
out vec3 gColor;
void main() {
    vec4 base = gl_in[0].gl_Position;
    float s = 0.2;
    gColor = vec3(0.8,0.5,0.2);
    gl_Position = base + vec4(-s, -s, 0.0, 0.0); EmitVertex();
    gl_Position = base + vec4( s, -s, 0.0, 0.0); EmitVertex();
    gl_Position = base + vec4(-s,  s, 0.0, 0.0); EmitVertex();
    gColor = vec3(0.6,0.8,0.9);
    gl_Position = base + vec4( s,  s, 0.0, 0.0); EmitVertex();
    gl_Position = base + vec4(0.0,  s*1.6, 0.0, 0.0); EmitVertex();
    EndPrimitive();
}")
                ]
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.07 0.07 0.08 1.0)
                (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
                (GL20/glUseProgram program)
                (GL30/glBindVertexArray (:vao points))
                (GL11/glDrawArrays GL11/GL_POINTS 0 (:count points))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          :geometry-shader-exploding
          (let [program (create-program*
                         "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
out vec3 Normal;
void main() {
    Normal = aNormal;
    gl_Position = vec4(aPos, 1.0);
}"
                         "#version 330 core
in vec3 NormalOut;
out vec4 FragColor;
void main() { FragColor = vec4(abs(normalize(NormalOut)), 1.0); }"
                         "#version 330 core
layout (triangles) in;
layout (triangle_strip, max_vertices = 3) out;
in vec3 Normal[];
out vec3 NormalOut;
uniform float time;
void main() {
    for(int i=0;i<3;i++) {
        vec4 pos = gl_in[i].gl_Position;
        pos.xyz += Normal[i] * (0.1 + 0.1 * sin(time));
        NormalOut = Normal[i];
        gl_Position = pos;
        EmitVertex();
    }
    EndPrimitive();
}")]
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.06 0.06 0.08 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL20/glUseProgram program)
                (GL20/glUniform1f (GL20/glGetUniformLocation program "time") (float (GLFW/glfwGetTime)))
                (GL30/glBindVertexArray (:vao cube))
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          :geometry-shader-normals
          (let [program (create-program*
                         "#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
out vec3 Normal;
out vec3 Pos;
void main() {
    Pos = aPos;
    Normal = aNormal;
    gl_Position = vec4(aPos, 1.0);
}"
                         "#version 330 core
in vec3 NormalOut;
out vec4 FragColor;
void main() { FragColor = vec4(NormalOut * 0.5 + 0.5, 1.0); }"
                         "#version 330 core
layout (triangles) in;
layout (line_strip, max_vertices = 6) out;
in vec3 Normal[];
in vec3 Pos[];
out vec3 NormalOut;
void main() {
    for(int i=0;i<3;i++) {
        vec3 p = Pos[i];
        NormalOut = vec3(1.0,1.0,1.0);
        gl_Position = vec4(p, 1.0); EmitVertex();
        gl_Position = vec4(p + Normal[i]*0.3, 1.0); EmitVertex();
        EndPrimitive();
    }
}")]
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.05 0.05 0.07 1.0)
                (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
                (GL20/glUseProgram program)
                (GL30/glBindVertexArray (:vao cube))
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program))

          (:instancing-quads :asteroids :asteroids-instanced)
          (let [quad-mesh (instanced-quad-mesh)
                inst-pos (if (= scenario :instancing-quads)
                           [[-1.0 -1.0 0.0] [-1.0 1.0 0.0] [1.0 -1.0 0.0] [1.0 1.0 0.0]]
                           (for [i (range 0 150)]
                             (let [angle (/ (* 2 Math/PI i) 150.0)
                                   radius (if (= scenario :asteroids) 5.0 6.5)
                                   offset (if (= scenario :asteroids-instanced) 0.6 0.3)]
                               [(+ (* radius (Math/cos angle)) (- offset (rand (* 2 offset))))
                                (+ (* radius (Math/sin angle)) (- offset (rand (* 2 offset))))
                                (- (rand) 0.5)])))
                instance-vbo (create-instance-offsets (:vao quad-mesh) inst-pos)
                program (create-program*
                         "#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aTex;
layout (location = 2) in vec3 aOffset;
out vec2 TexCoord;
void main() {
    TexCoord = aTex;
    vec2 pos = aPos * 0.3 + aOffset.xy;
    gl_Position = vec4(pos, aOffset.z, 1.0);
}"
                         "#version 330 core
in vec2 TexCoord;
out vec4 FragColor;
void main() {
    FragColor = vec4(0.8, 0.9, 0.2, 1.0) * (0.5 + 0.5 * TexCoord.y);
}")]
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (GL11/glClearColor 0.02 0.02 0.03 1.0)
                (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
                (GL20/glUseProgram program)
                (GL30/glBindVertexArray (:vao quad-mesh))
                (GL31/glDrawElementsInstanced GL11/GL_TRIANGLES (:count quad-mesh) GL11/GL_UNSIGNED_INT 0 (count inst-pos))
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (GL20/glDeleteProgram program)
            (GL15/glDeleteBuffers (:vbo quad-mesh))
            (GL15/glDeleteBuffers instance-vbo)
            (GL15/glDeleteBuffers (:ebo quad-mesh))
            (GL30/glDeleteVertexArrays (:vao quad-mesh)))

          (:anti-aliasing-msaa :anti-aliasing-offscreen)
          (let [program (simple-program)
                model (Matrix4f.)
                view (basic-view)
                proj (basic-projection)
                fb (when (= scenario :anti-aliasing-offscreen) (setup-msaa-framebuffer))
                model-loc (GL20/glGetUniformLocation program "model")
                view-loc (GL20/glGetUniformLocation program "view")
                proj-loc (GL20/glGetUniformLocation program "projection")
                color-loc (GL20/glGetUniformLocation program "baseColor")]
            (GL11/glEnable GL13/GL_MULTISAMPLE)
            (GL11/glEnable GL11/GL_DEPTH_TEST)
            (loop []
              (when-not (GLFW/glfwWindowShouldClose window)
                (when fb
                  (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER (:fbo fb)))
                (GL11/glClearColor 0.08 0.08 0.1 1.0)
                (GL11/glClear (bit-or GL11/GL_COLOR_BUFFER_BIT GL11/GL_DEPTH_BUFFER_BIT))
                (GL20/glUseProgram program)
                (upload-mat! proj mat-buf proj-loc)
                (upload-mat! view mat-buf view-loc)
                (GL30/glBindVertexArray (:vao cube))
                (.identity model)
                (.rotate model (float (GLFW/glfwGetTime)) 0.3 0.5 0.2)
                (upload-mat! model mat-buf model-loc)
                (GL20/glUniform3f color-loc 0.6 0.9 0.5)
                (GL11/glDrawArrays GL11/GL_TRIANGLES 0 (:count cube))
                (when fb
                  (GL30/glBindFramebuffer GL30/GL_READ_FRAMEBUFFER (:fbo fb))
                  (GL30/glBindFramebuffer GL30/GL_DRAW_FRAMEBUFFER 0)
                  (GL30/glBlitFramebuffer 0 0 width height 0 0 width height GL11/GL_COLOR_BUFFER_BIT GL11/GL_NEAREST))
                (GL30/glBindFramebuffer GL30/GL_FRAMEBUFFER 0)
                (GLFW/glfwSwapBuffers window)
                (GLFW/glfwPollEvents)
                (recur)))
            (when fb
              (GL30/glDeleteRenderbuffers (:rbo fb))
              (GL30/glDeleteRenderbuffers (:depth fb))
              (GL30/glDeleteFramebuffers (:fbo fb)))
            (GL20/glDeleteProgram program)))
        (GL15/glDeleteBuffers (:vbo cube))
        (GL30/glDeleteVertexArrays (:vao cube))
        (GL15/glDeleteBuffers (:vbo plane))
        (GL30/glDeleteVertexArrays (:vao plane))
        (GL15/glDeleteBuffers (:vbo quad))
        (GL30/glDeleteVertexArrays (:vao quad))
        (GL15/glDeleteBuffers (:vbo discard-quad))
        (GL30/glDeleteVertexArrays (:vao discard-quad))
        (GL15/glDeleteBuffers (:vbo points))
        (GL30/glDeleteVertexArrays (:vao points)))
      (finally
        (cleanup-window env)))))

(defn run-example!
  [scenario]
  (run-scene! scenario))

(defn -main
  [& [scenario]]
  (let [kw (if scenario (keyword scenario) :depth-testing)]
    (run-example! kw)))
