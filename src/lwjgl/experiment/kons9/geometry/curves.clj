(ns lwjgl.experiment.kons9.geometry.curves
  "参数化曲线扩展 - 对应 kons-9 的 plugins/parametric-curve.lisp"
  (:require [lwjgl.experiment.kons9.math :as math]))

;; ============================================
;; Bezier 曲线
;; ============================================

(defn bezier-cubic-point
  "计算三次 Bezier 曲线上的点"
  [t p0 p1 p2 p3]
  (let [u (- 1.0 t)
        tt (* t t)
        uu (* u u)
        uuu (* uu u)
        ttt (* tt t)]
    (if (number? p0)
      (+ (* uuu p0)
         (* 3.0 uu t p1)
         (* 3.0 u tt p2)
         (* ttt p3))
      (mapv #(bezier-cubic-point t %1 %2 %3 %4) p0 p1 p2 p3))))

(defn make-bezier-curve
  "创建三次 Bezier 曲线点列表"
  [p0 p1 p2 p3 num-points]
  (mapv #(bezier-cubic-point (/ % (double (dec num-points))) p0 p1 p2 p3)
        (range num-points)))

;; ============================================
;; Catmull-Rom 样条
;; ============================================

(defn catmull-rom-point
  "Catmull-Rom 样条插值"
  [t p0 p1 p2 p3]
  (let [t2 (* t t)
        t3 (* t2 t)
        b0 (+ (* -0.5 t3) t2 (* -0.5 t))
        b1 (+ (* 1.5 t3) (* -2.5 t2) 1.0)
        b2 (+ (* -1.5 t3) (* 2.0 t2) (* 0.5 t))
        b3 (- (* 0.5 t3) (* 0.5 t2))]
    (if (number? p0)
      (+ (* b0 p0) (* b1 p1) (* b2 p2) (* b3 p3))
      (mapv #(catmull-rom-point t %1 %2 %3 %4) p0 p1 p2 p3))))

(defn make-catmull-rom-curve
  "创建 Catmull-Rom 样条曲线"
  [control-points num-points-per-seg]
  (let [n (count control-points)
        closed? (= (first control-points) (last control-points))
        get-point (fn [i]
                    (cond
                      closed? (nth control-points (mod i (dec n)))
                      (< i 0) (first control-points)
                      (>= i n) (last control-points)
                      :else (nth control-points i)))]
    (if (< n 2)
      control-points
      (vec (mapcat (fn [i]
                     (mapv (fn [j]
                             (let [t (/ j (double num-points-per-seg))]
                               (catmull-rom-point t
                                                  (get-point (dec i))
                                                  (get-point i)
                                                  (get-point (inc i))
                                                  (get-point (+ i 2)))))
                           (range num-points-per-seg)))
                   (range (if closed? (dec n) (dec n))))))))

;; ============================================
;; 数学曲线
;; ============================================

(defn make-circle-points
  "创建圆形点列表"
  [radius num-points]
  (mapv (fn [i]
          (let [angle (* 2.0 math/PI (/ i (double num-points)))]
            [(* radius (Math/cos angle))
             0.0
             (* radius (Math/sin angle))]))
        (range num-points)))

(defn make-helix-points
  "创建螺旋线点列表"
  [radius height turns num-points]
  (let [total-points (* turns num-points)]
    (mapv (fn [i]
            (let [t (/ i (double total-points))
                  angle (* 2.0 math/PI turns t)
                  y (- (* height t) (/ height 2.0))]
              [(* radius (Math/cos angle))
               y
               (* radius (Math/sin angle))]))
          (range total-points))))

(defn make-spiral-points
  "创建平面螺旋线"
  [start-radius end-radius num-points]
  (mapv (fn [i]
          (let [t (/ i (double num-points))
                angle (* 4.0 math/PI t)
                r (+ start-radius (* (- end-radius start-radius) t))]
            [(* r (Math/cos angle))
             0.0
             (* r (Math/sin angle))]))
        (range num-points)))
