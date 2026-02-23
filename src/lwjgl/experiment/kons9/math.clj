(ns lwjgl.experiment.kons9.math
  "核心数学工具函数"
  (:import (org.joml Matrix4f)))

;; ============================================
;; 常量
;; ============================================

(def PI Math/PI)
(def TAU (* 2.0 Math/PI))

;; ============================================
;; 基础数学
;; ============================================

(defn clamp
  "将值限制在 [min, max] 范围内"
  [v min-val max-val]
  (max (double min-val) (min (double max-val) (double v))))

(defn lerp
  "线性插值"
  [a b t]
  (+ a (* (- b a) t)))

(defn radians
  "角度转弧度"
  [deg]
  (Math/toRadians (double deg)))

;; ============================================
;; 向量运算 (3D)
;; ============================================

(defn vadd
  "向量加法 (支持多参数)"
  ([a] a)
  ([a b]
   (let [[ax ay az] a [bx by bz] b]
     [(+ ax bx) (+ ay by) (+ az bz)]))
  ([a b & more]
   (reduce vadd (vadd a b) more)))

(defn vsub
  "向量减法"
  [[ax ay az] [bx by bz]]
  [(- ax bx) (- ay by) (- az bz)])

(defn vmul
  "向量逐分量相乘"
  [[ax ay az] [bx by bz]]
  [(* ax bx) (* ay by) (* az bz)])

(defn vmul
  "向量标量乘法"
  [[x y z] s]
  [(* x s) (* y s) (* z s)])

(defn vdiv
  "向量标量除法"
  [[x y z] d]
  (if (zero? d)
    [0.0 0.0 0.0]
    [(/ x d) (/ y d) (/ z d)]))

(defn dot
  "向量点积"
  [[ax ay az] [bx by bz]]
  (+ (* ax bx) (* ay by) (* az bz)))

(defn cross
  "向量叉积"
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn len-sq
  "向量长度平方"
  [[x y z]]
  (+ (* x x) (* y y) (* z z)))

(defn len
  "向量长度"
  [v]
  (Math/sqrt (len-sq v)))

(defn normal
  "向量归一化"
  [v]
  (let [l (len v)]
    (if (< l 0.0001)
      [0.0 1.0 0.0]
      (vdiv v l))))

(defn dist
  "两点距离"
  [a b]
  (len (vsub a b)))

(defn dist-sq
  "两点距离平方"
  [a b]
  (len-sq (vsub a b)))

;; ============================================
;; 实用函数
;; ============================================

(defn wrap
  "循环包裹值"
  [v min-val max-val]
  (let [range (- max-val min-val)]
    (if (<= range 0)
      min-val
      (let [wrapped (mod (- v min-val) range)]
        (+ min-val (if (< wrapped 0) (+ wrapped range) wrapped))))))

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
