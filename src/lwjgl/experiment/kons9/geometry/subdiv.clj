(ns lwjgl.experiment.kons9.geometry.subdiv
  "细分网格 - Catmull-Clark/Loop 算法"
  (:require [lwjgl.experiment.kons9.math :as math]))

;; ============================================
;; Catmull-Clark 细分
;; ============================================

(defn- edge-key
  "创建边的唯一键"
  [i j]
  (if (< i j) [i j] [j i]))

(defn- face-center
  "计算面的重心"
  [vertices face]
  (let [points (map #(nth vertices %) face)]
    (math/vmul (reduce math/vadd points) (/ 1.0 (count face)))))

(defn- edge-midpoint
  "计算边的中点"
  [vertices edge]
  (let [[i j] edge
        p1 (nth vertices i)
        p2 (nth vertices j)]
    (math/vmul (math/vadd p1 p2) 0.5)))

(defn subdivide-catmull-clark
  "Catmull-Clark 细分算法"
  [vertices faces]
  (let [n-verts (count vertices)

        ; 1. 计算面点
        face-points (mapv #(face-center vertices %) faces)

        ; 2. 构建边到面的映射
        edge-faces (atom {})
        _ (doseq [[f-idx face] (map-indexed vector faces)]
            (doseq [i (range (count face))]
              (let [j (mod (inc i) (count face))
                    edge (edge-key (nth face i) (nth face j))]
                (swap! edge-faces update edge conj f-idx))))

        ; 3. 计算边点
        edge-points (into {} (map (fn [[edge f-idxs]]
                                    (let [mid (edge-midpoint vertices edge)
                                          face-avg (if (= 2 (count f-idxs))
                                                     (math/vmul (math/vadd (nth face-points (first f-idxs))
                                                                           (nth face-points (second f-idxs)))
                                                                0.5)
                                                     (nth face-points (first f-idxs)))]
                                      [edge (math/vmul (math/vadd mid face-avg) 0.5)]))
                                  @edge-faces))

        ; 4. 计算新顶点位置
        new-vertices (mapv (fn [v-idx]
                             (let [adj-faces (keep-indexed (fn [f-idx face]
                                                             (when (some #(= % v-idx) face)
                                                               f-idx))
                                                           faces)
                                   adj-edges (keep (fn [[edge _]]
                                                     (when (some #(= % v-idx) edge)
                                                       edge))
                                                   @edge-faces)
                                   n (count adj-faces)]
                               (if (zero? n)
                                 (nth vertices v-idx)
                                 (let [f (math/vmul (reduce math/vadd (map #(nth face-points %) adj-faces))
                                                    (/ 1.0 n))
                                       r (math/vmul (reduce math/vadd (map #(edge-midpoint vertices %) adj-edges))
                                                    (/ 1.0 n))
                                       p (nth vertices v-idx)]
                                   (math/vdiv (math/vadd (math/vmul f 1.0)
                                                         (math/vmul r 2.0)
                                                         (math/vmul p (- n 3)))
                                              (double n))))))
                           (range n-verts))

        ; 5. 构建新网格
        all-verts (vec (concat new-vertices face-points (vals edge-points)))
        edge-offset (+ n-verts (count face-points))
        edge-index-map (into {} (map-indexed (fn [idx edge]
                                               [edge (+ edge-offset idx)])
                                             (keys edge-points)))

        ; 6. 构建新面
        new-faces (vec (mapcat (fn [f-idx face]
                                 (let [face-point-idx (+ n-verts f-idx)
                                       n (count face)
                                       edges (map #(edge-key (nth face %) (nth face (mod (inc %) n)))
                                                  (range n))]
                                   (map (fn [i]
                                          (let [v (nth face i)
                                                e1 (nth edges i)
                                                e2 (nth edges (mod (dec i) n))]
                                            [v (get edge-index-map e2) face-point-idx (get edge-index-map e1)]))
                                        (range n))))
                               (range (count faces))
                               faces))]

    {:vertices all-verts :faces new-faces}))

;; ============================================
;; Loop 细分 (三角形)
;; ============================================

(defn- triangulate-face
  "将多边形面三角化"
  [face]
  (if (<= (count face) 3)
    [face]
    (let [v0 (first face)
          rest-verts (rest face)]
      (map (fn [[v1 v2]] [v0 v1 v2])
           (partition 2 1 rest-verts)))))

(defn subdivide-loop
  "Loop 细分算法 (适用于三角形网格)"
  [vertices faces]
  ; 先三角化
  (let [tri-faces (vec (mapcat triangulate-face faces))
        n-verts (count vertices)

        ; 计算边点
        edge-points (atom {})
        _ (doseq [face tri-faces]
            (doseq [i (range 3)]
              (let [j (mod (inc i) 3)
                    edge (edge-key (nth face i) (nth face j))]
                (when-not (contains? @edge-points edge)
                  (let [p1 (nth vertices (nth face i))
                        p2 (nth vertices (nth face j))
                        ; 找到共享这条边的另一个三角形的顶点
                        other-verts (mapcat (fn [f]
                                              (when (and (not= f face)
                                                         (some #(= % (nth face i)) f)
                                                         (some #(= % (nth face j)) f))
                                                (filter #(and (not= % (nth face i)) (not= % (nth face j))) f)))
                                            tri-faces)]
                    (if (= 2 (count other-verts))
                      (let [opposite-avg (math/vmul (math/vadd (nth vertices (first other-verts))
                                                               (nth vertices (second other-verts)))
                                                    0.5)]
                        (swap! edge-points assoc edge
                               (math/vmul (math/vadd (math/vmul p1 0.375)
                                                     (math/vmul p2 0.375)
                                                     (math/vmul opposite-avg 0.25))
                                          1.0)))
                      (swap! edge-points assoc edge (math/vmul (math/vadd p1 p2) 0.5))))))))

        ; 更新原顶点
        new-vertices (mapv (fn [v-idx]
                             (let [adj-verts (atom [])
                                   _ (doseq [face tri-faces]
                                       (when (some #(= % v-idx) face)
                                         (swap! adj-verts conj
                                                (some #(when (and (not= % v-idx)) %) face))))]
                               (if (< (count @adj-verts) 3)
                                 (nth vertices v-idx)
                                 (let [n (count @adj-verts)
                                       beta (if (= n 3) 3/16 (- 3/8 n))
                                       neighbor-sum (reduce math/vadd (map #(nth vertices %) @adj-verts))]
                                   (math/vadd (math/vmul (nth vertices v-idx) (- 1.0 (* n beta)))
                                              (math/vmul neighbor-sum beta))))))
                           (range n-verts))

        ; 构建新网格
        all-verts (vec (concat new-vertices (vals @edge-points)))
        edge-offset n-verts
        edge-index-map (into {} (map-indexed (fn [idx edge]
                                               [edge (+ edge-offset idx)])
                                             (keys @edge-points)))

        ; 每个三角形变成4个
        new-faces (vec (mapcat (fn [face]
                                 (let [[v0 v1 v2] face
                                       e0 (get edge-index-map (edge-key v0 v1))
                                       e1 (get edge-index-map (edge-key v1 v2))
                                       e2 (get edge-index-map (edge-key v2 v0))]
                                   [[v0 e0 e2]
                                    [v1 e1 e0]
                                    [v2 e2 e1]
                                    [e0 e1 e2]]))
                               tri-faces))]

    {:vertices all-verts :faces new-faces}))

;; ============================================
;; 多次细分
;; ============================================

(defn subdivide-times
  "应用多次细分"
  [vertices faces subdivide-fn iterations]
  (loop [v vertices
         f faces
         i 0]
    (if (>= i iterations)
      {:vertices v :faces f}
      (let [result (subdivide-fn v f)]
        (recur (:vertices result) (:faces result) (inc i))))))
