(ns lwjgl.experiment.kons9.math
  (:import (org.joml Matrix4f)))

(defn clamp01
  "将数值限制在 0-1 范围内。"
  [v]
  (min 1.0 (max 0.0 v)))

(defn vec3
  "创建三维向量。"
  [x y z]
  [(float x) (float y) (float z)])

(defn color3
  "创建颜色，自动限制在 0-1 范围内。"
  [r g b]
  [(float (clamp01 r)) (float (clamp01 g)) (float (clamp01 b))])

(def ^:private rainbow-stops
  [[1.0 0.0 0.0]
   [1.0 0.6 0.0]
   [1.0 1.0 0.0]
   [0.0 1.0 0.0]
   [0.0 1.0 1.0]
   [0.0 0.0 1.0]
   [0.5 0.0 1.0]])

(defn rainbow-color
  "根据输入 0-1 值返回彩虹渐变颜色。"
  [t]
  (let [t (mod (double t) 1.0)
        stops rainbow-stops
        max-idx (dec (count stops))
        scaled (* t max-idx)
        idx (int (Math/floor scaled))
        frac (clamp01 (- scaled idx))
        idx (min idx max-idx)
        idx-next (min (inc idx) max-idx)
        c1 (nth stops idx)
        c2 (nth stops idx-next)
        mix (fn [a b]
              (+ a (* frac (- b a))))]
    (color3 (mix (nth c1 0) (nth c2 0))
            (mix (nth c1 1) (nth c2 1))
            (mix (nth c1 2) (nth c2 2)))))

(defn- ensure-vec3
  "保证返回长度为 3 的向量，缺省值自动补齐。"
  [v default]
  (let [v (vec (or v []))]
    [(float (or (get v 0) (get default 0)))
     (float (or (get v 1) (get default 1)))
     (float (or (get v 2) (get default 2)))]))

(defn compose-transform
  "根据位移、旋转、缩放生成变换矩阵。"
  [pos rot scale]
  (let [[px py pz] (ensure-vec3 pos [0.0 0.0 0.0])
        [rx ry rz] (ensure-vec3 rot [0.0 0.0 0.0])
        [sx sy sz] (ensure-vec3 scale [1.0 1.0 1.0])]
    (doto (Matrix4f.)
      (.identity)
      (.translate px py pz)
      (.rotateX (float (Math/toRadians rx)))
      (.rotateY (float (Math/toRadians ry)))
      (.rotateZ (float (Math/toRadians rz)))
      (.scale sx sy sz))))

(defn chain-transform
  "在父矩阵基础上叠加位移、旋转与缩放。"
  [parent pos rot scale]
  (let [[px py pz] (ensure-vec3 pos [0.0 0.0 0.0])
        [rx ry rz] (ensure-vec3 rot [0.0 0.0 0.0])
        [sx sy sz] (ensure-vec3 scale [1.0 1.0 1.0])]
    (doto (Matrix4f. parent)
      (.translate px py pz)
      (.rotateX (float (Math/toRadians rx)))
      (.rotateY (float (Math/toRadians ry)))
      (.rotateZ (float (Math/toRadians rz)))
      (.scale sx sy sz))))

(defn rand-range
  "生成范围内的随机值。"
  [a b]
  (+ a (* (rand) (- b a))))
