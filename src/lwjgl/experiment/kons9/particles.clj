(ns lwjgl.experiment.kons9.particles
  (:require [lwjgl.experiment.kons9.math :as math]))

(defn- vadd
  [[ax ay az] [bx by bz]]
  [(+ ax bx) (+ ay by) (+ az bz)])

(defn- vsub
  [[ax ay az] [bx by bz]]
  [(- ax bx) (- ay by) (- az bz)])

(defn- vscale
  [[x y z] s]
  [(* x s) (* y s) (* z s)])

(defn- vlen
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn- vnormalize
  [v]
  (let [len (vlen v)]
    (if (<= len 1.0e-6)
      [0.0 0.0 0.0]
      (vscale v (/ 1.0 len)))))

(defn- vlimit
  [v max-len]
  (let [len (vlen v)]
    (if (<= len max-len)
      v
      (vscale v (/ max-len len)))))

(defn- rand-range
  [a b]
  (math/rand-range a b))

(defn- rand-vec
  [scale]
  [(rand-range (- scale) scale)
   (rand-range (- scale) scale)
   (rand-range (- scale) scale)])

(defn- noise3
  "简易噪声函数。"
  [x y z]
  (let [v (+ (* 12.9898 x) (* 78.233 y) (* 37.719 z))]
    (- (* 2.0 (- (mod (* (Math/sin v) 43758.5453) 1.0) 0.5)))))

(defn make-gravity-field
  "创建重力场。"
  [g]
  (fn [_ _] [0.0 (- (double g)) 0.0]))

(defn make-attractor-field
  "创建吸引场。"
  [center strength]
  (fn [pos _]
    (let [dir (vsub center pos)
          dist (max 1.0e-3 (vlen dir))
          acc (* (double strength) (/ 1.0 (+ 0.2 (* dist dist))))]
      (vscale (vnormalize dir) acc))))

(defn make-vortex-field
  "创建旋涡场。axis 默认为 Y 轴。"
  ([center strength]
   (make-vortex-field center [0.0 1.0 0.0] strength))
  ([center axis strength]
   (fn [pos _]
     (let [[ax ay az] (vnormalize axis)
           dir (vsub pos center)
           [dx dy dz] dir
           vx (- (* ay dz) (* az dy))
           vy (- (* az dx) (* ax dz))
           vz (- (* ax dy) (* ay dx))
           dist (max 1.0e-3 (vlen dir))
           acc (* (double strength) (/ 1.0 (+ 0.4 (* dist dist))))]
       (vscale [vx vy vz] acc)))))

(defn make-noise-field
  "创建噪声场。"
  [scale strength]
  (fn [[x y z] _]
    (let [sx (* (double scale) x)
          sy (* (double scale) y)
          sz (* (double scale) z)
          nx (noise3 (+ sx 3.1) sy sz)
          ny (noise3 sx (+ sy 2.7) sz)
          nz (noise3 sx sy (+ sz 5.2))]
      (vscale [nx ny nz] (double strength)))))

(defn make-particle
  "创建粒子。"
  [{:keys [pos vel life color size trail]
    :or {life 3.0 color [0.8 0.9 1.0] size 12.0}}]
  {:pos pos
   :vel vel
   :life (double life)
   :age 0.0
   :color color
   :size (double size)
   :trail (vec (or trail []))})

(defn- default-color
  []
  [(float (rand)) (float (rand)) (float (rand)) 0.85])

(defn emit-from-points
  "从点集发射粒子。支持 :direction-fn、:tangent?、:speed-range、:life-range、:size-range。"
  [points & {:keys [direction-fn tangent? speed-range life-range size-range color-fn jitter]
             :or {speed-range [0.3 0.9]
                  life-range [1.5 4.0]
                  size-range [10.0 18.0]
                  jitter 0.0}}]
  (let [speed-range (mapv double speed-range)
        life-range (mapv double life-range)
        size-range (mapv double size-range)]
    (mapv
     (fn [[x y z :as p]]
       (let [dir (cond
                   direction-fn (direction-fn p)
                   tangent? (let [t [(- y) x 0.0]] (if (< (vlen t) 1.0e-6) [1.0 0.0 0.0] t))
                   :else p)
             dir (vnormalize dir)
             speed (rand-range (first speed-range) (second speed-range))
             vel (vadd (vscale dir speed) (rand-vec jitter))
             life (rand-range (first life-range) (second life-range))
             size (rand-range (first size-range) (second size-range))
             color (if color-fn (color-fn p) (default-color))]
         (make-particle {:pos p :vel vel :life life :color color :size size})))
     (vec points))))

(defn update-particles-advanced
  "更新粒子集合，支持力场与碰撞。"
  [particles dt {:keys [fields ground-y bounce drag max-speed trail-length respawn-fn]
                 :or {ground-y -1.2 bounce 0.4 drag 0.02 max-speed 2.0 trail-length 18}}]
  (let [fields (or fields [])
        dt (double dt)]
    (mapv
     (fn [{:keys [pos vel life age] :as p}]
       (if (>= age life)
         (if respawn-fn (respawn-fn p) p)
         (let [acc (reduce (fn [a f] (vadd a (f pos vel))) [0.0 0.0 0.0] fields)
               vel (vadd vel (vscale acc dt))
               vel (vscale vel (- 1.0 (* drag dt)))
               vel (vlimit vel max-speed)
               pos (vadd pos (vscale vel dt))
               [x y z] pos
               [vx vy vz] vel
               hit? (<= y ground-y)
               pos (if hit? [x ground-y z] pos)
               vel (if hit? [vx (* (- bounce) vy) vz] vel)
               trail (conj (vec (take-last (max 0 (dec trail-length)) (:trail p))) pos)]
           (assoc p
                  :pos pos
                  :vel vel
                  :age (+ age dt)
                  :trail trail))))
     particles)))

(defn particles->sprites
  "将粒子集合转换为精灵数据。"
  [particles]
  {:points (mapv :pos particles)
   :colors (mapv :color particles)
   :sizes (mapv :size particles)})

(defn particles->trails
  "将粒子轨迹转换为线段数据。"
  [particles]
  (let [data (transient [])]
    (doseq [{:keys [trail color]} particles]
      (when (>= (count trail) 2)
        (let [[r g b a] (if (vector? color) (conj (subvec (vec color) 0 3) (or (nth color 3) 1.0)) [1.0 1.0 1.0 1.0])]
          (doseq [[p0 p1] (partition 2 1 trail)]
            (let [[x1 y1 z1] p0
                  [x2 y2 z2] p1]
              (conj! data (float x1))
              (conj! data (float y1))
              (conj! data (float z1))
              (conj! data (float r))
              (conj! data (float g))
              (conj! data (float b))
              (conj! data (float x2))
              (conj! data (float y2))
              (conj! data (float z2))
              (conj! data (float r))
              (conj! data (float g))
              (conj! data (float b)))))))
    (persistent! data)))

(defn- respawn-particle
  [radius]
  (let [angle (* 2.0 Math/PI (rand))
        r (math/rand-range (* 0.2 radius) radius)
        x (* r (Math/cos angle))
        z (* r (Math/sin angle))
        y (math/rand-range -0.2 0.2)
        vx (math/rand-range -0.3 0.3)
        vy (math/rand-range 0.1 0.6)
        vz (math/rand-range -0.3 0.3)]
    {:pos [(float x) (float y) (float z)]
     :vel [(float vx) (float vy) (float vz)]
     :life (math/rand-range 1.5 4.0)
     :color [(float (rand)) (float (rand)) (float (rand))]}))

(defn init-particles
  "初始化粒子集合。"
  [count radius]
  (vec (repeatedly (max 1 (int count)) #(respawn-particle radius))))

(defn update-particles
  "更新粒子集合，返回新的粒子状态。"
  [particles dt t]
  (let [target [(float (* 0.5 (Math/sin (* 0.6 t))))
                (float (* 0.3 (Math/cos (* 0.4 t))))
                0.0]
        gravity [0.0 -0.25 0.0]]
    (mapv
     (fn [{:keys [pos vel life] :as p}]
       (let [[px py pz] pos
             [vx vy vz] vel
             [tx ty tz] target
             dx (- tx px)
             dy (- ty py)
             dz (- tz pz)
             dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 0.001))
             att-scale (/ 0.6 (+ 0.2 (* dist dist)))
             ax (+ (nth gravity 0) (* att-scale (/ dx dist)) (* 0.15 (- pz)))
             ay (+ (nth gravity 1) (* att-scale (/ dy dist)))
             az (+ (nth gravity 2) (* att-scale (/ dz dist)) (* 0.15 px))
             nvx (+ vx (* ax dt))
             nvy (+ vy (* ay dt))
             nvz (+ vz (* az dt))
             npx (+ px (* nvx dt))
             npy (+ py (* nvy dt))
             npz (+ pz (* nvz dt))
             life (- life dt)]
         (if (or (<= life 0.0) (> (Math/abs npx) 4.0) (> (Math/abs npy) 4.0) (> (Math/abs npz) 4.0))
           (respawn-particle 1.6)
           (assoc p :pos [(float npx) (float npy) (float npz)]
                  :vel [(float nvx) (float nvy) (float nvz)]
                  :life life))))
     particles)))

(defn particles->points
  "将粒子集合转换为点云数据。"
  [particles]
  (let [points (mapv :pos particles)
        colors (mapv :color particles)]
    {:points points :colors colors}))
