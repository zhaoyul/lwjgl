(ns lwjgl.experiment.kons9.geometry.sweep
  "扫描网格 - Sweep/Loft/Extrusion"
  (:require [lwjgl.experiment.kons9.math :as math]))

(defn make-sweep-mesh
  "沿路径扫描轮廓生成网格"
  [profile path & {:keys [twist-fn scale-fn]
                   :or {twist-fn (constantly 0.0)
                        scale-fn (constantly 1.0)}}]
  (let [n-profile (count profile)
        n-path (count path)
        path-tangents (mapv (fn [i]
                              (let [prev (nth path (max 0 (dec i)))
                                    next-idx (min (dec n-path) (inc i))
                                    next-pt (nth path next-idx)]
                                (math/normal (math/vsub next-pt prev))))
                            (range n-path))
        profile-center (math/vmul (reduce math/vadd profile) (/ 1.0 n-profile))
        vertices (atom [])
        faces (atom [])]

    (doseq [i (range n-path)]
      (let [point (nth path i)
            tangent (nth path-tangents i)
            up (if (< (Math/abs (math/dot tangent [0.0 1.0 0.0])) 0.99)
                 [0.0 1.0 0.0]
                 [0.0 0.0 1.0])
            axis1 (math/normal (math/cross up tangent))
            axis2 (math/cross tangent axis1)
            twist (twist-fn (/ i (double (dec n-path))))
            scale (scale-fn (/ i (double (dec n-path))))
            cos-t (Math/cos twist)
            sin-t (Math/sin twist)]

        (doseq [j (range n-profile)]
          (let [profile-point (nth profile j)
                local-point (math/vmul (math/vsub profile-point profile-center) scale)
                px (+ (* (first local-point) cos-t)
                      (* (second local-point) (- sin-t)))
                py (+ (* (first local-point) sin-t)
                      (* (second local-point) cos-t))
                pz (nth local-point 2)
                world-point (-> (math/vmul axis1 px)
                                (math/vadd (math/vmul axis2 py))
                                (math/vadd (math/vmul tangent pz))
                                (math/vadd point))]
            (swap! vertices conj world-point)))))

    (doseq [i (range (dec n-path))
            j (range n-profile)
            :let [j1 (mod (inc j) n-profile)
                  base0 (* i n-profile)
                  base1 (* (inc i) n-profile)]]
      (swap! faces conj [(+ base0 j) (+ base0 j1) (+ base1 j1) (+ base1 j)]))

    {:vertices @vertices :faces @faces}))

(defn make-extrusion
  "挤出轮廓"
  [profile length direction segments]
  (let [dir-norm (math/normal direction)
        path (mapv (fn [i]
                     (math/vmul dir-norm (* length (/ i (double segments)))))
                   (range (inc segments)))]
    (make-sweep-mesh profile path)))

(defn make-revolution
  "创建旋转曲面"
  [profile segments & {:keys [start-angle end-angle]
                       :or {start-angle 0.0
                            end-angle 360.0}}]
  (let [start-rad (math/radians start-angle)
        end-rad (math/radians end-angle)
        angle-range (- end-rad start-rad)
        n-profile (count profile)
        vertices (atom [])
        faces (atom [])]

    (doseq [i (range (inc segments))]
      (let [angle (+ start-rad (* angle-range (/ i (double segments))))
            cos-a (Math/cos angle)
            sin-a (Math/sin angle)]
        (doseq [j (range n-profile)]
          (let [[px py _] (nth profile j)]
            (swap! vertices conj [(* px cos-a) py (* px sin-a)])))))

    (doseq [i (range segments)
            j (range n-profile)
            :let [j1 (mod (inc j) n-profile)
                  base0 (* i n-profile)
                  base1 (* (inc i) n-profile)]]
      (swap! faces conj [(+ base0 j) (+ base0 j1) (+ base1 j1) (+ base1 j)]))

    {:vertices @vertices :faces @faces}))

(defn make-pipe
  "创建管道"
  [path radius sides]
  (let [circle (mapv (fn [i]
                       (let [angle (* 2.0 math/PI (/ i (double sides)))]
                         [(* radius (Math/cos angle))
                          (* radius (Math/sin angle))
                          0.0]))
                     (range sides))]
    (make-sweep-mesh circle path)))

(defn make-spring
  "创建弹簧"
  [coil-radius wire-radius turns height segments-per-turn sides]
  (let [total-segments (* turns segments-per-turn)
        path (mapv (fn [i]
                     (let [t (/ i (double total-segments))
                           angle (* 2.0 math/PI turns t)
                           y (- (* height t) (/ height 2.0))]
                       [(* coil-radius (Math/cos angle))
                        y
                        (* coil-radius (Math/sin angle))]))
                   (range (inc total-segments)))
        circle (mapv (fn [i]
                       (let [angle (* 2.0 math/PI (/ i (double sides)))]
                         [(* wire-radius (Math/cos angle))
                          (* wire-radius (Math/sin angle))
                          0.0]))
                     (range sides))]
    (make-sweep-mesh circle path)))
