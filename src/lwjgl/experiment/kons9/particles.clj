(ns lwjgl.experiment.kons9.particles
  (:require [lwjgl.experiment.kons9.math :as math]))

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
