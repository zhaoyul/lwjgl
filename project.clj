(defproject lwjgl "0.1.0-SNAPSHOT"
  :description "LWJGL instanced rectangles"
  :dependencies [[org.clojure/clojure "1.12.0"]          ;; 更新: 1.11.1 -> 1.12.0
                 [org.lwjgl/lwjgl "3.3.6"]               ;; 更新: 3.3.3 -> 3.3.6
                 [org.lwjgl/lwjgl-glfw "3.3.6"]
                 [org.lwjgl/lwjgl-opengl "3.3.6"]
                 [org.lwjgl/lwjgl "3.3.6" :classifier "natives-macos-arm64"]
                 [org.lwjgl/lwjgl-glfw "3.3.6" :classifier "natives-macos-arm64"]
                 [org.lwjgl/lwjgl-opengl "3.3.6" :classifier "natives-macos-arm64"]
                 [org.joml/joml "1.10.8"]]
  :profiles {:dev {:dependencies [[nrepl "1.1.0"]]}} ;; REPL control
  :source-paths ["src"]
  :resource-paths ["resources"]
  :main lwjgl.core
  :jvm-opts ["-XstartOnFirstThread"
             "--enable-native-access=ALL-UNNAMED"])
