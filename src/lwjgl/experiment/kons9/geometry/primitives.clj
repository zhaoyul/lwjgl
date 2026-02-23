(ns lwjgl.experiment.kons9.geometry.primitives
  "基础几何体生成 - 对应 kons-9 的 kernel 和 plugins"
  (:require [lwjgl.experiment.kons9.math :as math]))

;; ============================================
;; 基础网格生成
;; ============================================

(defn make-box-mesh-data
  "创建立方体顶点和面数据"
  [width height depth]
  (let [hw (/ width 2.0)
        hh (/ height 2.0)
        hd (/ depth 2.0)
        vertices [[(- hw) (- hh) (- hd)]
                  [hw (- hh) (- hd)]
                  [hw hh (- hd)]
                  [(- hw) hh (- hd)]
                  [(- hw) (- hh) hd]
                  [hw (- hh) hd]
                  [hw hh hd]
                  [(- hw) hh hd]]
        faces [[0 1 2 3]  ; 底
               [4 7 6 5]  ; 顶
               [0 4 5 1]  ; 前
               [2 6 7 3]  ; 后
               [0 3 7 4]  ; 左
               [1 5 6 2]]] ; 右
    {:vertices vertices :faces faces}))

(defn make-cylinder-mesh-data
  "创建圆柱体顶点和面数据"
  [radius height segments]
  (let [angle-step (/ (* 2.0 math/PI) segments)
        ; 底部和顶部圆环顶点
        bottom-verts (mapv (fn [i]
                             (let [angle (* i angle-step)]
                               [(* radius (Math/cos angle))
                                (- (/ height 2.0))
                                (* radius (Math/sin angle))]))
                           (range segments))
        top-verts (mapv (fn [i]
                          (let [angle (* i angle-step)]
                            [(* radius (Math/cos angle))
                             (/ height 2.0)
                             (* radius (Math/sin angle))]))
                        (range segments))
        vertices (concat bottom-verts top-verts)
        ; 侧面面
        side-faces (mapv (fn [i]
                           (let [next-i (mod (inc i) segments)]
                             [i next-i (+ next-i segments) (+ i segments)]))
                         (range segments))
        ; 底面和顶面 (三角扇)
        bottom-face (vec (range segments))
        top-face (vec (map #(+ % segments) (range segments)))]
    {:vertices (vec vertices)
     :faces (concat side-faces [bottom-face top-face])}))

(defn make-sphere-mesh-data
  "创建 UV 球体顶点和面数据"
  [radius u-segments v-segments]
  (let [vertices (vec (for [v (range (inc v-segments))]
                        (vec (for [u (range u-segments)]
                               (let [phi (* 2.0 math/PI (/ u (double u-segments)))
                                     theta (* math/PI (/ v (double v-segments)))
                                     x (* radius (Math/sin theta) (Math/cos phi))
                                     y (* radius (Math/cos theta))
                                     z (* radius (Math/sin theta) (Math/sin phi))]
                                 [x y z])))))
        ; 扁平化顶点
        flat-verts (vec (apply concat vertices))
        ; 构建面
        faces (vec (for [v (range v-segments)
                         u (range u-segments)
                         :let [u0 u
                               u1 (mod (inc u) u-segments)
                               v0 (+ (* v u-segments) u0)
                               v1 (+ (* v u-segments) u1)
                               v2 (+ (* (inc v) u-segments) u1)
                               v3 (+ (* (inc v) u-segments) u0)]
                         :when (< v v-segments)]
                     (if (or (= v 0) (= v v-segments))
                      ; 极点是三角形
                       (if (= v 0)
                         [v0 v2 v3]
                         [v0 v1 v2])
                      ; 中间是四边形
                       [v0 v1 v2 v3])))]
    {:vertices flat-verts :faces faces}))

(defn make-torus-mesh-data
  "创建圆环体顶点和面数据"
  [major-radius minor-radius u-segments v-segments]
  (let [vertices (vec (for [u (range u-segments)]
                        (vec (for [v (range v-segments)]
                               (let [phi (* 2.0 math/PI (/ u (double u-segments)))
                                     theta (* 2.0 math/PI (/ v (double v-segments)))
                                     x (* (Math/cos phi) (+ major-radius (* minor-radius (Math/cos theta))))
                                     y (* minor-radius (Math/sin theta))
                                     z (* (Math/sin phi) (+ major-radius (* minor-radius (Math/cos theta))))]
                                 [x y z])))))
        flat-verts (vec (apply concat vertices))
        faces (vec (for [u (range u-segments)
                         v (range v-segments)
                         :let [u0 u
                               u1 (mod (inc u) u-segments)
                               v0 v
                               v1 (mod (inc v) v-segments)
                               i0 (+ (* u0 v-segments) v0)
                               i1 (+ (* u0 v-segments) v1)
                               i2 (+ (* u1 v-segments) v1)
                               i3 (+ (* u1 v-segments) v0)]]
                     [i0 i1 i2 i3]))]
    {:vertices flat-verts :faces faces}))

;; ============================================
;; 超级二次曲面
;; ============================================

(defn make-superquadric-mesh-data
  "创建超级二次曲面"
  [radius e1 e2 u-segments v-segments]
  (let [vertices (vec (for [u (range u-segments)]
                        (vec (for [v (range (inc v-segments))]
                               (let [phi (* 2.0 math/PI (/ u (double u-segments)))
                                     theta (* math/PI (/ v (double v-segments)))
                                   ; 超级二次曲面公式
                                     cos-phi (Math/cos phi)
                                     sin-phi (Math/sin phi)
                                     cos-theta (Math/cos theta)
                                     sin-theta (Math/sin theta)
                                     cu (* (Math/pow (Math/abs cos-phi) e1)
                                           (if (< cos-phi 0) -1 1))
                                     su (* (Math/pow (Math/abs sin-phi) e1)
                                           (if (< sin-phi 0) -1 1))
                                     cv (* (Math/pow (Math/abs cos-theta) e2)
                                           (if (< cos-theta 0) -1 1))
                                     sv (* (Math/pow (Math/abs sin-theta) e2)
                                           (if (< sin-theta 0) -1 1))
                                     r (/ radius 2.0)]
                                 (if (or (= v 0) (= v v-segments))
                                   [0.0 (* sv r) 0.0]
                                   [(* r cv cu)
                                    (* r sv)
                                    (* r cv su)]))))))
        flat-verts (vec (apply concat vertices))
        faces (vec (for [u (range u-segments)
                         v (range v-segments)
                         :let [u0 u
                               u1 (mod (inc u) u-segments)
                               v0 v
                               v1 (inc v)
                               i0 (+ (* u0 (inc v-segments)) v0)
                               i1 (+ (* u0 (inc v-segments)) v1)
                               i2 (+ (* u1 (inc v-segments)) v1)
                               i3 (+ (* u1 (inc v-segments)) v0)]]
                     [i0 i1 i2 i3]))]
    {:vertices flat-verts :faces faces}))
