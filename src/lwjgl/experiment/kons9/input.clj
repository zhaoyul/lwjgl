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
  "注册默认快捷键：1-9 切换场景，0 启动演示，P 暂停/恢复，H 切换高度场扫掠。"
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
  (register-fn GLFW/GLFW_KEY_H GLFW/GLFW_PRESS :heightfield-sweep)
  (register-fn GLFW/GLFW_KEY_0 GLFW/GLFW_PRESS :demo)
  (register-fn GLFW/GLFW_KEY_P GLFW/GLFW_PRESS :pause)
  :ok)
