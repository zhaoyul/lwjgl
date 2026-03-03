;; 提取 deps.edn 中指定 alias 的信息，用于 Linux 运行
;; 用法: clj -M scripts/extract-alias-info.clj <alias-name> <natives-alias>
;; 输出格式: MAIN_CLASS|CLASSPATH|JVM_OPT1,JVM_OPT2,... 或 FALLBACK

(require '[clojure.edn :as edn])
(require '[clojure.java.shell :as shell])
(require '[clojure.string :as str])

(let [args *command-line-args*
      main-alias (first args)
      natives-alias (second args)
      deps (edn/read-string (slurp "deps.edn"))
      alias-kw (keyword main-alias)
      alias-config (get-in deps [:aliases alias-kw])]
  
  (if-not alias-config
    (do
      (binding [*out* *err*]
        (println "Error: Unknown alias" main-alias))
      (System/exit 1))
    
    (let [main-opts (:main-opts alias-config)
          jvm-opts (filter #(not= "-XstartOnFirstThread" %) (:jvm-opts alias-config))]
      
      (if (and main-opts (= "-m" (first main-opts)))
        ;; 有 main-opts 且是 -m 模式
        (let [main-class (second main-opts)
              _ (binding [*out* *err*] (println "Getting classpath..."))
              classpath-result (shell/sh "clj" "-Spath" (str "-M" natives-alias))
              classpath (str/trim (:out classpath-result))]
          
          (if (not= 0 (:exit classpath-result))
            (do
              (binding [*out* *err*]
                (println "FAILED:" (:err classpath-result)))
              (System/exit 1))
            ;; 成功获取 classpath，输出结果
            (do
              (println (str main-class "|" classpath "|" (str/join "," jvm-opts)))
              (System/exit 0))))
        ;; 没有 main-opts 或不是 -m 模式，输出 FALLBACK
        (do
          (println "FALLBACK")
          (System/exit 0))))))
