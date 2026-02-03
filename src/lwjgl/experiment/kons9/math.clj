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

(defn compose-transform
  "根据位移、旋转、缩放生成变换矩阵。"
  [pos rot scale]
  (doto (Matrix4f.)
    (.identity)
    (.translate (float (nth pos 0)) (float (nth pos 1)) (float (nth pos 2)))
    (.rotateX (float (Math/toRadians (nth rot 0))))
    (.rotateY (float (Math/toRadians (nth rot 1))))
    (.rotateZ (float (Math/toRadians (nth rot 2))))
    (.scale (float (nth scale 0)) (float (nth scale 1)) (float (nth scale 2)))))

(defn chain-transform
  "在父矩阵基础上叠加位移、旋转与缩放。"
  [parent pos rot scale]
  (doto (Matrix4f. parent)
    (.translate (float (nth pos 0)) (float (nth pos 1)) (float (nth pos 2)))
    (.rotateX (float (Math/toRadians (nth rot 0))))
    (.rotateY (float (Math/toRadians (nth rot 1))))
    (.rotateZ (float (Math/toRadians (nth rot 2))))
    (.scale (float (nth scale 0)) (float (nth scale 1)) (float (nth scale 2)))))

(defn rand-range
  "生成范围内的随机值。"
  [a b]
  (+ a (* (rand) (- b a))))
