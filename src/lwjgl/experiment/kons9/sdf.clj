(ns lwjgl.experiment.kons9.sdf
  (:require [lwjgl.experiment.kons9.math :as math]))

(declare marching-tetrahedra)

(defn- sdf-sphere
  [x y z [cx cy cz] r]
  (let [dx (- x cx)
        dy (- y cy)
        dz (- z cz)]
    (- (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz))) r)))

(defn- smooth-min
  "平滑最小值（k 越大越平滑）。"
  [^double a ^double b ^double k]
  (let [h (math/clamp01 (+ 0.5 (/ (- b a) (* 2.0 k))))]
    (- (min a b) (* k h (- 1.0 h)))))

(defn sdf-metaballs
  "Metaballs 的 SDF 函数。"
  [^double x ^double y ^double z t]
  (let [c1 [(+ -0.6 (* 0.6 (Math/sin (* 0.7 t))))
            (* 0.4 (Math/cos (* 0.5 t)))
            0.0]
        c2 [(+ 0.6 (* 0.4 (Math/sin (* 0.9 t))))
            (* -0.3 (Math/cos (* 0.6 t)))
            0.2]
        c3 [0.0 (* 0.5 (Math/sin (* 0.4 t))) -0.4]
        d1 (sdf-sphere x y z c1 0.6)
        d2 (sdf-sphere x y z c2 0.55)
        d3 (sdf-sphere x y z c3 0.5)]
    (-> d1
        (smooth-min d2 0.4)
        (smooth-min d3 0.4))))

(defn- sdf-gradient
  [f x y z t]
  (let [e 0.002
        dx (- (f (+ x e) y z t) (f (- x e) y z t))
        dy (- (f x (+ y e) z t) (f x (- y e) z t))
        dz (- (f x y (+ z e) t) (f x y (- z e) t))
        len (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-9))]
    [(/ dx len) (/ dy len) (/ dz len)]))

(defn- dist2
  [[x y z] [ax ay az]]
  (let [dx (- x ax)
        dy (- y ay)
        dz (- z az)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- segment-distance-sq
  "点到线段距离平方."
  [[x y z] [ax ay az] [bx by bz]]
  (let [abx (- bx ax)
        aby (- by ay)
        abz (- bz az)
        apx (- x ax)
        apy (- y ay)
        apz (- z az)
        ab2 (+ (* abx abx) (* aby aby) (* abz abz) 1.0e-9)
        t (math/clamp01 (/ (+ (* apx abx) (* apy aby) (* apz abz)) ab2))
        cx (+ ax (* t abx))
        cy (+ ay (* t aby))
        cz (+ az (* t abz))
        dx (- x cx)
        dy (- y cy)
        dz (- z cz)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn field-from-points
  "从点集生成标量场函数."
  [points & {:keys [radius strength] :or {radius 0.35 strength 1.0}}]
  (let [points (vec points)
        r (double radius)
        r2 (* r r)
        s (double strength)]
    (fn [x y z _]
      (let [p [x y z]]
        (reduce (fn [acc pt]
                  (let [d2 (dist2 p pt)]
                    (+ acc (/ (* s r2) (+ r2 d2)))))
                0.0
                points)))))

(defn field-from-curves
  "从曲线点集生成标量场函数."
  [curves & {:keys [radius strength] :or {radius 0.35 strength 1.0}}]
  (let [segments (->> curves
                      (mapcat #(partition 2 1 %))
                      (vec))
        r (double radius)
        r2 (* r r)
        s (double strength)]
    (fn [x y z _]
      (let [p [x y z]]
        (reduce (fn [acc [a b]]
                  (let [d2 (segment-distance-sq p a b)]
                    (+ acc (/ (* s r2) (+ r2 d2)))))
                0.0
                segments)))))

(defn field->sdf
  "将标量场转换为等值面 SDF."
  [field threshold]
  (let [threshold (double threshold)]
    (fn [x y z t]
      (- threshold (field x y z t)))))

(defn isosurface-from-field
  "从标量场生成等值面网格."
  [field {:keys [threshold min max res t]
          :or {threshold 1.0
               min [-1.4 -1.1 -1.1]
               max [1.4 1.1 1.1]
               res 36
               t 0.0}}]
  (marching-tetrahedra (field->sdf field threshold) {:min min :max max :res res :t t}))

(defn isosurface-from-points
  "从点集生成等值面网格."
  [points & {:as opts}]
  (let [field (apply field-from-points points (mapcat identity opts))]
    (isosurface-from-field field opts)))

(defn isosurface-from-curves
  "从曲线点集生成等值面网格."
  [curves & {:as opts}]
  (let [field (apply field-from-curves curves (mapcat identity opts))]
    (isosurface-from-field field opts)))

(defn sphere-sdf
  "生成以 center 为中心、radius 为半径的 SDF。"
  [center radius]
  (let [[cx cy cz] (mapv double center)
        r (double radius)]
    (fn [x y z _]
      (sdf-sphere x y z [cx cy cz] r))))

(defn box-sdf
  "生成轴对齐尺寸为 size 的盒子 SDF."
  [center size]
  (let [[cx cy cz] (mapv double center)
        half (mapv #(Math/abs (/ (double %) 2.0)) size)]
    (fn [x y z _]
      (let [px (- x cx)
            py (- y cy)
            pz (- z cz)
            qx (- (Math/abs px) (first half))
            qy (- (Math/abs py) (second half))
            qz (- (Math/abs pz) (nth half 2))
            out-x (max qx 0.0)
            out-y (max qy 0.0)
            out-z (max qz 0.0)
            outside (Math/sqrt (+ (* out-x out-x)
                                  (* out-y out-y)
                                  (* out-z out-z)))
            inside (min (max qx (max qy qz)) 0.0)]
        (+ outside inside)))))

(defn torus-sdf
  "生成以 center 为中心的圆环 SDF。major 为主半径，minor 为截面半径。"
  [center major minor]
  (let [[cx cy cz] (mapv double center)
        r1 (double major)
        r2 (double minor)]
    (fn [x y z _]
      (let [px (- x cx)
            py (- y cy)
            pz (- z cz)
            qx (Math/sqrt (+ (* px px) (* pz pz)))
            dx (- qx r1)]
        (- (Math/sqrt (+ (* dx dx) (* py py)))
           r2)))))

(defn translate-sdf
  "将 SDF 沿 offset 平移."
  [offset sdf]
  (let [[ox oy oz] (mapv double offset)]
    (fn [x y z t]
      (sdf (- x ox) (- y oy) (- z oz) t))))

(defn sdf-union
  "返回两个 SDF 的并集, 可选平滑半径."
  ([a b] (sdf-union a b 0.0))
  ([a b ^double k]
   (if (<= k 0.0)
     (fn [x y z t]
       (min (a x y z t) (b x y z t)))
     (fn [x y z t]
       (smooth-min (a x y z t) (b x y z t) k)))))

(defn sdf-intersect
  "返回两个 SDF 的交集, 可选平滑半径."
  ([a b] (sdf-intersect a b 0.0))
  ([a b ^double k]
   (fn [x y z t]
     (- (smooth-min (- (a x y z t)) (- (b x y z t)) k)))))

(defn sdf-difference
  "从 a 中减去 b, 可选平滑半径."
  ([a b] (sdf-difference a b 0.0))
  ([a b ^double k]
   (fn [x y z t]
     (- (smooth-min (- (a x y z t)) (b x y z t) k)))))

(defn isosurface-from-sdf
  "直接从 SDF 生成等值面网格."
  [sdf & {:keys [min max res t]
          :or {min [-1.4 -1.1 -1.1]
               max [1.4 1.1 1.1]
               res 40
               t 0.0}}]
  (marching-tetrahedra sdf {:min min :max max :res res :t t}))

(def ^:private tetra-indices
  [[0 5 1 6]
   [0 1 2 6]
   [0 2 3 6]
   [0 3 7 6]
   [0 7 4 6]
   [0 4 5 6]])

(def ^:private cube-offsets
  [[0 0 0]
   [1 0 0]
   [1 1 0]
   [0 1 0]
   [0 0 1]
   [1 0 1]
   [1 1 1]
   [0 1 1]])

(defn- interp-point
  [p1 p2 v1 v2]
  (let [t (if (= v1 v2) 0.5 (/ (- 0.0 v1) (- v2 v1)))]
    [(+ (nth p1 0) (* t (- (nth p2 0) (nth p1 0))))
     (+ (nth p1 1) (* t (- (nth p2 1) (nth p1 1))))
     (+ (nth p1 2) (* t (- (nth p2 2) (nth p1 2))))]))

(defn- marching-tetra
  [points values f t vertices indices]
  (let [inside (mapv #(< % 0.0) values)
        count-in (count (filter true? inside))]
    (cond
      (= count-in 0) nil
      (= count-in 4) nil
      (= count-in 1)
      (let [idx (.indexOf inside true)
            others (vec (remove #(= % idx) [0 1 2 3]))
            p0 (nth points idx)
            v0 (nth values idx)
            pts (mapv (fn [i] (interp-point p0 (nth points i) v0 (nth values i))) others)
            base (quot (count @vertices) 6)
            order [[0 1 2]]
            crosses pts]
        (doseq [p crosses]
          (let [[nx ny nz] (sdf-gradient f (nth p 0) (nth p 1) (nth p 2) t)]
            (swap! vertices into [(float (nth p 0)) (float (nth p 1)) (float (nth p 2))
                                  (float nx) (float ny) (float nz)])))
        (swap! indices into
               (mapcat (fn [[a b c]] [(+ base a) (+ base b) (+ base c)]) order)))
      (= count-in 3)
      (let [idx (.indexOf inside false)
            others (vec (remove #(= % idx) [0 1 2 3]))
            p0 (nth points idx)
            v0 (nth values idx)
            pts (mapv (fn [i] (interp-point p0 (nth points i) v0 (nth values i))) others)
            base (quot (count @vertices) 6)
            order [[0 2 1]]
            crosses pts]
        (doseq [p crosses]
          (let [[nx ny nz] (sdf-gradient f (nth p 0) (nth p 1) (nth p 2) t)]
            (swap! vertices into [(float (nth p 0)) (float (nth p 1)) (float (nth p 2))
                                  (float nx) (float ny) (float nz)])))
        (swap! indices into
               (mapcat (fn [[a b c]] [(+ base a) (+ base b) (+ base c)]) order)))
      (= count-in 2)
      (let [inside-idx (keep-indexed (fn [i v] (when v i)) inside)
            outside-idx (keep-indexed (fn [i v] (when (not v) i)) inside)
            [a b] inside-idx
            [c d] outside-idx
            p-a (nth points a)
            p-b (nth points b)
            p-c (nth points c)
            p-d (nth points d)
            v-a (nth values a)
            v-b (nth values b)
            v-c (nth values c)
            v-d (nth values d)
            p-ac (interp-point p-a p-c v-a v-c)
            p-ad (interp-point p-a p-d v-a v-d)
            p-bc (interp-point p-b p-c v-b v-c)
            p-bd (interp-point p-b p-d v-b v-d)
            base (quot (count @vertices) 6)
            order [[0 1 2] [1 3 2]]
            crosses [p-ac p-ad p-bc p-bd]]
        (doseq [p crosses]
          (let [[nx ny nz] (sdf-gradient f (nth p 0) (nth p 1) (nth p 2) t)]
            (swap! vertices into [(float (nth p 0)) (float (nth p 1)) (float (nth p 2))
                                  (float nx) (float ny) (float nz)])))
        (swap! indices into
               (mapcat (fn [[a b c]] [(+ base a) (+ base b) (+ base c)]) order)))
      :else nil)))

(defn marching-tetrahedra
  "使用四面体分割的 marching 算法生成网格。"
  [f {:keys [min max res t]}]
  (let [[minx miny minz] min
        [maxx maxy maxz] max
        res (clojure.core/max 4 (int res))
        step-x (/ (- maxx minx) res)
        step-y (/ (- maxy miny) res)
        step-z (/ (- maxz minz) res)
        vertices (atom [])
        indices (atom [])]
    (dotimes [i res]
      (dotimes [j res]
        (dotimes [k res]
          (let [cube-points (mapv (fn [[ox oy oz]]
                                    [(+ minx (* (+ i ox) step-x))
                                     (+ miny (* (+ j oy) step-y))
                                     (+ minz (* (+ k oz) step-z))])
                                  cube-offsets)
                cube-values (mapv (fn [[x y z]] (f x y z t)) cube-points)]
            (doseq [[a b c d] tetra-indices]
              (marching-tetra [(nth cube-points a)
                               (nth cube-points b)
                               (nth cube-points c)
                               (nth cube-points d)]
                              [(nth cube-values a)
                               (nth cube-values b)
                               (nth cube-values c)
                               (nth cube-values d)]
                              f t vertices indices))))))
    {:vertices (float-array @vertices)
     :indices (int-array @indices)}))
