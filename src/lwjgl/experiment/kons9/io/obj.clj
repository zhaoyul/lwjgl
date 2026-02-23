(ns lwjgl.experiment.kons9.io.obj
  "OBJ 文件导入/导出"
  (:require [clojure.string :as str]))

;; ============================================
;; OBJ 导入
;; ============================================

(defn- parse-obj-line
  "解析 OBJ 文件的一行"
  [line]
  (let [trimmed (str/trim line)]
    (when (and (not (str/blank? trimmed))
               (not (str/starts-with? trimmed "#")))
      (let [parts (str/split trimmed #"\s+")
            cmd (first parts)]
        (case cmd
          "v" {:type :vertex
               :coord [(Double/parseDouble (nth parts 1))
                       (Double/parseDouble (nth parts 2))
                       (Double/parseDouble (nth parts 3))]}
          "vn" {:type :normal
                :normal [(Double/parseDouble (nth parts 1))
                         (Double/parseDouble (nth parts 2))
                         (Double/parseDouble (nth parts 3))]}
          "f" {:type :face
               :verts (mapv (fn [part]
                              (let [indices (str/split part #"/")
                                    v-idx (dec (Integer/parseInt (first indices)))]
                                v-idx))
                            (rest parts))}
          nil)))))

(defn import-obj
  "导入 OBJ 文件，返回顶点列表和面列表"
  [filepath]
  (let [vertices (atom [])
        faces (atom [])
        normals (atom [])]
    (with-open [reader (clojure.java.io/reader filepath)]
      (doseq [line (line-seq reader)]
        (when-let [parsed (parse-obj-line line)]
          (case (:type parsed)
            :vertex (swap! vertices conj (:coord parsed))
            :normal (swap! normals conj (:normal parsed))
            :face (swap! faces conj (:verts parsed))
            nil))))
    {:vertices @vertices
     :faces @faces
     :normals @normals}))

;; ============================================
;; OBJ 导出
;; ============================================

(defn export-obj
  "导出为 OBJ 格式字符串"
  [vertices faces & {:keys [name]
                     :or {name "exported"}}]
  (with-out-str
    (println (str "# " name))
    (println)
    (doseq [[x y z] vertices]
      (println "v" x y z))
    (println)
    (doseq [face faces]
      (print "f")
      (doseq [idx face]
        (print " " (inc idx)))
      (println))))

(defn export-obj-to-file
  "导出 OBJ 到文件"
  [vertices faces filepath & {:keys [name]
                              :or {name "exported"}}]
  (spit filepath (export-obj vertices faces :name name)))
