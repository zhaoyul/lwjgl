(ns lwjgl.experiment.kons9.input
  (:import (org.lwjgl.glfw GLFW)))

(defonce command-table (atom {}))
(defonce key-handler (atom nil))

(defn register-command!
  "注册键盘命令。action 可以是 GLFW/GLFW_PRESS 等。"
  [key action f]
  (swap! command-table assoc [key action] f))

(defn set-key-handler!
  "设置键盘处理函数。"
  [f]
  (reset! key-handler f))

(defn handle-key
  "处理键盘事件。"
  [window key action]
  (when-let [cmd (get @command-table [key action])]
    (cmd window key action))
  (when-let [f @key-handler]
    (f window key action)))

(defn register-demo-commands!
  "注册默认快捷键：1-9 切换场景，0 启动演示，P 暂停/恢复，H/J 切换扩展场景。"
  [register-fn]
  (register-fn GLFW/GLFW_KEY_1 GLFW/GLFW_PRESS :geometry)
  (register-fn GLFW/GLFW_KEY_2 GLFW/GLFW_PRESS :rig)
  (register-fn GLFW/GLFW_KEY_3 GLFW/GLFW_PRESS :sweep)
  (register-fn GLFW/GLFW_KEY_4 GLFW/GLFW_PRESS :heightfield)
  (register-fn GLFW/GLFW_KEY_5 GLFW/GLFW_PRESS :particles)
  (register-fn GLFW/GLFW_KEY_6 GLFW/GLFW_PRESS :sdf)
  (register-fn GLFW/GLFW_KEY_7 GLFW/GLFW_PRESS :spring)
  (register-fn GLFW/GLFW_KEY_8 GLFW/GLFW_PRESS :ecosystem)
  (register-fn GLFW/GLFW_KEY_9 GLFW/GLFW_PRESS :heightfield-particles)
  (register-fn GLFW/GLFW_KEY_K GLFW/GLFW_PRESS :surface-particles)
  (register-fn GLFW/GLFW_KEY_H GLFW/GLFW_PRESS :heightfield-sweep)
  (register-fn GLFW/GLFW_KEY_J GLFW/GLFW_PRESS :heightfield-animate)
  (register-fn GLFW/GLFW_KEY_L GLFW/GLFW_PRESS :polyhedron-particles)
  (register-fn GLFW/GLFW_KEY_U GLFW/GLFW_PRESS :polyhedron-vertices)
  (register-fn GLFW/GLFW_KEY_I GLFW/GLFW_PRESS :polyhedron-face-centers)
  (register-fn GLFW/GLFW_KEY_R GLFW/GLFW_PRESS :polyhedron-refine)
  (register-fn GLFW/GLFW_KEY_F GLFW/GLFW_PRESS :polyhedron-fractal)
  (register-fn GLFW/GLFW_KEY_C GLFW/GLFW_PRESS :cut-cube)
  (register-fn GLFW/GLFW_KEY_B GLFW/GLFW_PRESS :box)
  (register-fn GLFW/GLFW_KEY_T GLFW/GLFW_PRESS :resource-growth)
  (register-fn GLFW/GLFW_KEY_O GLFW/GLFW_PRESS :isosurface-points)
  (register-fn GLFW/GLFW_KEY_M GLFW/GLFW_PRESS :isosurface-curves)
  (register-fn GLFW/GLFW_KEY_N GLFW/GLFW_PRESS :isosurface-particles)
  (register-fn GLFW/GLFW_KEY_Y GLFW/GLFW_PRESS :spring-isosurface)
  (register-fn GLFW/GLFW_KEY_0 GLFW/GLFW_PRESS :demo)
  (register-fn GLFW/GLFW_KEY_P GLFW/GLFW_PRESS :pause)
  :ok)
