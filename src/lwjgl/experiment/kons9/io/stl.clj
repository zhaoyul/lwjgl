(ns lwjgl.experiment.kons9.io.stl
  "STL 文件导入/导出"
  (:require [lwjgl.experiment.kons9.math :as math])
  (:import [java.nio ByteBuffer ByteOrder]
           [java.io FileInputStream FileOutputStream]))

;; ============================================
;; 二进制 STL 读取
;; ============================================

(defn- read-float-le
  "读取小端浮点数"
  [^ByteBuffer buf]
  (.getFloat buf))

(defn- parse-binary-stl
  "解析二进制 STL 文件"
  [^bytes data]
  (let [buf (ByteBuffer/wrap data)
        _ (.order buf ByteOrder/LITTLE_ENDIAN)
        _ (.position buf 80)  ; 跳过头部
        num-triangles (bit-and (.getInt buf) 0xFFFFFFFF)

        vertices (atom [])
        faces (atom [])]

    (dotimes [_ num-triangles]
      ; 跳过法线
      (dotimes [_ 3] (read-float-le buf))
      ; 读取 3 个顶点
      (let [base-idx (count @vertices)
            v1 [(read-float-le buf) (read-float-le buf) (read-float-le buf)]
            v2 [(read-float-le buf) (read-float-le buf) (read-float-le buf)]
            v3 [(read-float-le buf) (read-float-le buf) (read-float-le buf)]]
        (swap! vertices conj v1 v2 v3)
        (swap! faces conj [base-idx (inc base-idx) (+ base-idx 2)]))
      ; 跳过属性
      (.getShort buf))

    {:vertices @vertices :faces @faces}))

;; ============================================
;; ASCII STL 读取
;; ============================================

(defn- parse-ascii-stl
  "解析 ASCII STL 文件"
  [content]
  (let [lines (clojure.string/split-lines content)
        vertices (atom [])
        faces (atom [])]

    (doseq [line lines]
      (let [trimmed (clojure.string/trim line)
            parts (clojure.string/split trimmed #"\s+")]
        (when (= (first parts) "vertex")
          (swap! vertices conj
                 [(Double/parseDouble (nth parts 1))
                  (Double/parseDouble (nth parts 2))
                  (Double/parseDouble (nth parts 3))]))))

    ; 构建面 (每3个顶点一个三角形)
    (dotimes [i (/ (count @vertices) 3)]
      (swap! faces conj [(* i 3) (inc (* i 3)) (+ (* i 3) 2)]))

    {:vertices @vertices :faces @faces}))

(defn- is-binary-stl?
  "检查是否是二进制 STL"
  [^bytes data]
  (if (< (count data) 84)
    false
    (let [buf (ByteBuffer/wrap data)
          _ (.order buf ByteOrder/LITTLE_ENDIAN)
          _ (.position buf 80)
          num-triangles (bit-and (.getInt buf) 0xFFFFFFFF)
          expected-size (+ 84 (* num-triangles 50))]
      (= (count data) expected-size))))

(defn import-stl
  "导入 STL 文件 (自动检测格式)"
  [filepath]
  (let [data (with-open [in (FileInputStream. filepath)]
               (let [buf (byte-array (.available in))]
                 (.read in buf)
                 buf))]
    (if (is-binary-stl? data)
      (parse-binary-stl data)
      (parse-ascii-stl (String. data "UTF-8")))))

;; ============================================
;; 二进制 STL 导出
;; ============================================

(defn- write-float-le
  "写入小端浮点数"
  [^ByteBuffer buf f]
  (.putFloat buf (float f)))

(defn export-stl-binary
  "导出二进制 STL 文件"
  [vertices faces filepath]
  (let [triangles (mapcat (fn [face]
                            (if (= 3 (count face))
                              [face]
                              (let [v0 (first face)]
                                (map (fn [[v1 v2]] [v0 v1 v2])
                                     (partition 2 1 (rest face))))))
                          faces)
        num-triangles (count triangles)
        buf-size (+ 84 (* num-triangles 50))
        buf (ByteBuffer/allocate buf-size)]

    (.order buf ByteOrder/LITTLE_ENDIAN)

    ; 80字节头部
    (.put buf (byte-array 80))

    ; 三角形数量
    (.putInt buf num-triangles)

    ; 写入三角形
    (doseq [[i0 i1 i2] triangles]
      (let [p0 (nth vertices i0)
            p1 (nth vertices i1)
            p2 (nth vertices i2)
            normal (math/normal (math/cross (math/vsub p1 p0) (math/vsub p2 p0)))]
        ; 法线
        (write-float-le buf (first normal))
        (write-float-le buf (second normal))
        (write-float-le buf (nth normal 2))
        ; 顶点
        (doseq [p [p0 p1 p2]]
          (write-float-le buf (first p))
          (write-float-le buf (second p))
          (write-float-le buf (nth p 2)))
        ; 属性
        (.putShort buf (short 0))))

    (with-open [out (FileOutputStream. filepath)]
      (.write out (.array buf)))))

;; ============================================
;; ASCII STL 导出
;; ============================================

(defn export-stl-ascii
  "导出 ASCII STL 文件"
  [vertices faces filepath & {:keys [name]
                              :or {name "exported"}}]
  (let [triangles (mapcat (fn [face]
                            (if (= 3 (count face))
                              [face]
                              (let [v0 (first face)]
                                (map (fn [[v1 v2]] [v0 v1 v2])
                                     (partition 2 1 (rest face))))))
                          faces)]

    (with-open [writer (clojure.java.io/writer filepath)]
      (.write writer (str "solid " name "\n"))

      (doseq [[i0 i1 i2] triangles]
        (let [p0 (nth vertices i0)
              p1 (nth vertices i1)
              p2 (nth vertices i2)
              normal (math/normal (math/cross (math/vsub p1 p0) (math/vsub p2 p0)))]
          (.write writer "  facet normal ")
          (.write writer (str (first normal) " " (second normal) " " (nth normal 2) "\n"))
          (.write writer "    outer loop\n")
          (doseq [p [p0 p1 p2]]
            (.write writer (str "      vertex " (first p) " " (second p) " " (nth p 2) "\n")))
          (.write writer "    endloop\n")
          (.write writer "  endfacet\n")))

      (.write writer (str "endsolid " name "\n")))))
