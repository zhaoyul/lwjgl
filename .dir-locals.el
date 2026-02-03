;; 自动检测 macOS 架构并选择对应的 LWJGL 本地库 alias
;; Intel Mac (x86_64) -> :natives-intel
;; Apple Silicon (arm64) -> :natives-arm
;;
;; 如果需要组合其他 aliases，修改下面的 extra-aliases 变量
;; 例如：(setq extra-aliases ":cider:dev")

((clojure-mode
  . ((eval . (let* ((arch (string-trim (shell-command-to-string "uname -m")))
                    (natives-alias (if (string= arch "arm64")
                                       ":natives-arm"
                                     ":natives-intel"))
                    (extra-aliases "")  ; 在这里添加其他 aliases，如 ":cider:dev"
                    (final-aliases (concat natives-alias extra-aliases)))
               (setq-local cider-clojure-cli-aliases final-aliases)
               (message "LWJGL: 检测到 %s 架构，使用 %s alias" arch final-aliases))))))
