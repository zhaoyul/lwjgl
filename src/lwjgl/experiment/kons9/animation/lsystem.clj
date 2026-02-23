(ns lwjgl.experiment.kons9.animation.lsystem
  "L-系统 - 分形植物生成"
  (:require [lwjgl.experiment.kons9.math :as math]))

(defn make-turtle
  "创建海龟"
  [pos heading]
  {:pos pos
   :heading heading
   :stack []
   :path [pos]})

(defn turtle-forward
  "海龟前进"
  [turtle dist]
  (let [new-pos (math/vadd (:pos turtle) (math/vmul (:heading turtle) dist))]
    (-> turtle
        (assoc :pos new-pos)
        (update :path conj new-pos))))

(defn turtle-yaw
  "海龟 Y 轴旋转"
  [turtle angle]
  (let [rad (math/radians angle)
        cos-a (Math/cos rad)
        sin-a (Math/sin rad)
        h (:heading turtle)
        new-heading [(+ (* (first h) cos-a) (* (nth h 2) sin-a))
                     (second h)
                     (- (* (nth h 2) cos-a) (* (first h) sin-a))]]
    (assoc turtle :heading (math/normal new-heading))))

(defn turtle-push
  "保存状态"
  [turtle]
  (update turtle :stack conj {:pos (:pos turtle) :heading (:heading turtle)}))

(defn turtle-pop
  "恢复状态"
  [turtle]
  (if (empty? (:stack turtle))
    turtle
    (let [saved (peek (:stack turtle))]
      (-> turtle
          (assoc :pos (:pos saved))
          (assoc :heading (:heading saved))
          (update :stack pop)
          (assoc :path [(:pos saved)])))))

(defn evolve-lsystem
  "演化 L-系统"
  [axiom rules iterations]
  (loop [genome axiom
         i 0]
    (if (>= i iterations)
      genome
      (let [new-genome (mapcat (fn [symbol]
                                 (get rules symbol [symbol]))
                               genome)]
        (recur new-genome (inc i))))))

(defn interpret-lsystem
  "解释 L-系统生成路径"
  [genome angle step-size]
  (loop [symbols genome
         turtle (make-turtle [0.0 0.0 0.0] [0.0 1.0 0.0])
         paths []]
    (if (empty? symbols)
      (conj paths (:path turtle))
      (let [symbol (first symbols)
            new-turtle (case symbol
                         :F (turtle-forward turtle step-size)
                         :f (let [new-pos (math/vadd (:pos turtle)
                                                     (math/vmul (:heading turtle) step-size))]
                              (-> turtle
                                  (assoc :pos new-pos)
                                  (assoc :path [new-pos])))
                         :plus (turtle-yaw turtle (- angle))
                         :minus (turtle-yaw turtle angle)
                         :push (turtle-push turtle)
                         :pop (turtle-pop turtle)
                         turtle)]
        (recur (rest symbols)
               new-turtle
               (if (= symbol :pop)
                 (conj paths (:path turtle))
                 paths))))))

(defn make-plant-2d
  "2D 植物"
  []
  {:axiom [:X]
   :rules {:X [:F :minus [:minus :X] :plus :X :plus :plus :F :F :plus :X]
           :F [:F :F]}
   :angle 25.0
   :step-size 1.0
   :iterations 4})

(defn make-tree-3d
  "3D 树"
  []
  {:axiom [:A]
   :rules {:A [:F :minus [:minus :A] :plus :A]
           :F [:F :F]}
   :angle 30.0
   :step-size 2.0
   :iterations 3})

(defn make-fern
  "蕨类植物"
  []
  {:axiom [:X]
   :rules {:X [:F :plus [:minus :X] :plus :X]
           :F [:F :F]}
   :angle 20.0
   :step-size 1.0
   :iterations 4})

(defn make-koch-curve
  "Koch 曲线"
  []
  {:axiom [:F]
   :rules {:F [:F :plus :F :minus :F :minus :F :plus :F]}
   :angle 90.0
   :step-size 1.0
   :iterations 3})

(defn lsystem-to-paths
  "将 L-系统定义转换为路径列表"
  [lsystem-def]
  (let [{:keys [axiom rules angle step-size iterations]} lsystem-def
        genome (evolve-lsystem axiom rules iterations)]
    (filterv #(> (count %) 1)
             (interpret-lsystem genome angle step-size))))
