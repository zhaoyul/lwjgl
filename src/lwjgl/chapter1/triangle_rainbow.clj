(ns lwjgl.chapter1.triangle-rainbow
  (:gen-class)
  (:require [lwjgl.core :as core])
  (:import (org.lwjgl BufferUtils)
           (org.lwjgl.glfw GLFW GLFWCursorPosCallbackI GLFWFramebufferSizeCallbackI
                           GLFWKeyCallbackI GLFWMouseButtonCallbackI GLFWWindowSizeCallbackI)
           (org.lwjgl.opengl GL GL11 GL15 GL20 GL30)))

(def ^:private vertex-shader-source
  "#version 330 core
layout (location = 0) in vec2 aPos;
layout (location = 1) in vec3 aColor;
out vec3 vColor;
out vec2 vPos;
uniform vec2 offset;
uniform float angle;
void main() {
    float c = cos(angle);
    float s = sin(angle);
    vec2 rotated = vec2(c * aPos.x - s * aPos.y, s * aPos.x + c * aPos.y);
    vec2 pos = rotated + offset;
    vColor = aColor;
    vPos = pos;
    gl_Position = vec4(pos, 0.0, 1.0);
}")

(def ^:private fragment-shader-source
  "#version 330 core
in vec3 vColor;
in vec2 vPos;
out vec4 FragColor;
uniform float time;
uniform float hueShift;
uniform float mixFactor;
vec3 rainbow(float t) {
    return 0.5 + 0.5 * sin(vec3(0.0, 2.094, 4.188) + t);
}
void main() {
    float k = hueShift + vPos.x * 2.5 + vPos.y * 3.0 + time * 0.6;
    vec3 grad = rainbow(k);
    vec3 color = mix(vColor, grad, mixFactor);
    FragColor = vec4(color, 1.0);
}")

(defn- delete-if-positive
  [id f]
  (when (pos? id) (f id)))

(defn- clamp
  [v min-v max-v]
  (max min-v (min max-v v)))

(defn- create-triangle
  []
  (let [vertices (float-array
                  [0.0 0.6   1.0 0.2 0.2
                   -0.65 -0.5  0.2 0.9 0.3
                   0.65 -0.5   0.2 0.4 1.0])
        vao (GL30/glGenVertexArrays)
        vbo (GL15/glGenBuffers)
        buf (BufferUtils/createFloatBuffer (alength vertices))
        stride (* 5 Float/BYTES)]
    (GL30/glBindVertexArray vao)
    (.put buf vertices)
    (.flip buf)
    (GL15/glBindBuffer GL15/GL_ARRAY_BUFFER vbo)
    (GL15/glBufferData GL15/GL_ARRAY_BUFFER buf GL15/GL_STATIC_DRAW)
    (GL20/glVertexAttribPointer 0 2 GL11/GL_FLOAT false stride 0)
    (GL20/glEnableVertexAttribArray 0)
    (GL20/glVertexAttribPointer 1 3 GL11/GL_FLOAT false stride (* 2 Float/BYTES))
    (GL20/glEnableVertexAttribArray 1)
    (GL30/glBindVertexArray 0)
    {:vao vao :vbo vbo}))

(defn run-example!
  []
  (let [width (atom 800)
        height (atom 600)
        fb-width (atom 800)
        fb-height (atom 600)
        offset (atom [0.0 0.0])
        angle (atom 0.0)
        mix-factor (atom 0.7)
        hue-shift (atom 0.0)
        cursor-x (atom 0.0)
        dragging? (atom false)
        last-time (atom 0.0)
        error-callback (core/init-glfw!)
        window (core/create-window @width @height "LearnOpenGL - Triangle Rainbow (LWJGL)")]
    (try
      (GL/createCapabilities)
      (core/init-viewport! window fb-width fb-height)
      (let [program (core/create-program vertex-shader-source fragment-shader-source)
            {:keys [vao vbo]} (create-triangle)
            offset-loc (GL20/glGetUniformLocation program "offset")
            angle-loc (GL20/glGetUniformLocation program "angle")
            time-loc (GL20/glGetUniformLocation program "time")
            hue-loc (GL20/glGetUniformLocation program "hueShift")
            mix-loc (GL20/glGetUniformLocation program "mixFactor")]
        (try
          (GLFW/glfwSetFramebufferSizeCallback
           window
           (reify GLFWFramebufferSizeCallbackI
             (invoke [_ _ w h]
               (reset! fb-width w)
               (reset! fb-height h)
               (GL11/glViewport 0 0 w h))))
          (GLFW/glfwSetWindowSizeCallback
           window
           (reify GLFWWindowSizeCallbackI
             (invoke [_ _ w h]
               (reset! width w)
               (reset! height h))))
          (GLFW/glfwSetKeyCallback
           window
           (reify GLFWKeyCallbackI
             (invoke [_ win key _ action _]
               (when (and (= key GLFW/GLFW_KEY_ESCAPE)
                          (= action GLFW/GLFW_PRESS))
                 (GLFW/glfwSetWindowShouldClose win true)))))
          (GLFW/glfwSetMouseButtonCallback
           window
           (reify GLFWMouseButtonCallbackI
             (invoke [_ _ button action _]
               (when (= button GLFW/GLFW_MOUSE_BUTTON_LEFT)
                 (reset! dragging? (= action GLFW/GLFW_PRESS))))))
        (GLFW/glfwSetCursorPosCallback
         window
         (reify GLFWCursorPosCallbackI
           (invoke [_ _ xpos ypos]
             (reset! cursor-x xpos)
             (when @dragging?
               (let [nx (- (* 2.0 (/ xpos (double @width))) 1.0)
                     ny (- 1.0 (* 2.0 (/ ypos (double @height))))]
                 (reset! offset [(float nx) (float ny)]))))))
        (GL20/glUseProgram program)
        (loop []
           (when-not (GLFW/glfwWindowShouldClose window)
             (let [now (GLFW/glfwGetTime)
                   dt (- now @last-time)
                   _ (reset! last-time now)
                   speed (* 1.1 dt)
                   rot-speed (* 2.0 dt)
                   [ox oy] @offset
                   ox (cond-> ox
                        (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_A)) (- speed)
                        (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_D)) (+ speed))
                   oy (cond-> oy
                        (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_W)) (+ speed)
                        (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_S)) (- speed))
                   new-angle (cond-> @angle
                               (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_Q)) (+ rot-speed)
                               (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_E)) (- rot-speed))
                   new-mix (cond-> @mix-factor
                             (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_Z)) (- (* 0.6 dt))
                             (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_X)) (+ (* 0.6 dt)))]
               (when (= GLFW/GLFW_PRESS (GLFW/glfwGetKey window GLFW/GLFW_KEY_R))
                 (reset! offset [0.0 0.0])
                 (reset! angle 0.0)
                 (reset! mix-factor 0.7))
               (reset! offset [(float (clamp ox -1.0 1.0))
                               (float (clamp oy -1.0 1.0))])
               (reset! angle (float new-angle))
               (reset! mix-factor (float (clamp new-mix 0.0 1.0)))
               (reset! hue-shift (float (* 6.283 (/ @cursor-x (double @width)))))
               (GL11/glClearColor 0.04 0.05 0.08 1.0)
               (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
               (when (<= 0 offset-loc)
                 (let [[tx ty] @offset]
                   (GL20/glUniform2f offset-loc (float tx) (float ty))))
               (when (<= 0 angle-loc)
                 (GL20/glUniform1f angle-loc (float @angle)))
               (when (<= 0 time-loc)
                 (GL20/glUniform1f time-loc (float now)))
               (when (<= 0 hue-loc)
                 (GL20/glUniform1f hue-loc (float @hue-shift)))
               (when (<= 0 mix-loc)
                 (GL20/glUniform1f mix-loc (float @mix-factor)))
               (GL30/glBindVertexArray vao)
               (GL11/glDrawArrays GL11/GL_TRIANGLES 0 3)
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
