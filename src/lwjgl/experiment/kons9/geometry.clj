(ns lwjgl.experiment.kons9.geometry
  (:require [lwjgl.experiment.kons9.math :as math]))

(def ^:private tau (* 2.0 Math/PI))

(defn- deg->rad
  "角度转弧度."
  [deg]
  (Math/toRadians (double deg)))

(defn- lerp
  "线性插值."
  [t a b]
  (+ a (* t (- b a))))

(defn- safe-frac
  "计算 0-1 范围内的分段比例."
  [i n]
  (if (<= n 0)
    0.0
    (/ (double i) (double n))))

(defn- smoothstep
  "平滑插值."
  [t]
  (let [t (double t)]
    (* t t (- 3.0 (* 2.0 t)))))

(defn- hash2
  "二维哈希噪声."
  [x y]
  (let [v (+ (* 127.1 x) (* 311.7 y))]
    (- (* 2.0 (- (mod (* (Math/sin v) 43758.5453) 1.0) 0.5)))))

(defn- noise2
  "二维平滑噪声."
  [x y]
  (let [xi (Math/floor (double x))
        yi (Math/floor (double y))
        xf (- (double x) xi)
        yf (- (double y) yi)
        u (smoothstep xf)
        v (smoothstep yf)
        v00 (hash2 xi yi)
        v10 (hash2 (inc xi) yi)
        v01 (hash2 xi (inc yi))
        v11 (hash2 (inc xi) (inc yi))
        x1 (lerp u v00 v10)
        x2 (lerp u v01 v11)]
    (lerp v x1 x2)))

(defn- fbm2
  "二维分形噪声."
  [x y octaves lacunarity gain]
  (loop [i 0
         amp 1.0
         freq 1.0
         sum 0.0]
    (if (>= i octaves)
      sum
      (recur (inc i)
             (* amp gain)
             (* freq lacunarity)
             (+ sum (* amp (noise2 (* x freq) (* y freq))))))))

(defn- vnormalize
  "向量归一化."
  [[x y z]]
  (let [len (Math/sqrt (+ (* x x) (* y y) (* z z)))]
    (if (<= len 1.0e-6)
      [0.0 1.0 0.0]
      [(/ x len) (/ y len) (/ z len)])))

(defn- vadd
  "向量相加."
  [[ax ay az] [bx by bz]]
  [(+ ax bx) (+ ay by) (+ az bz)])

(defn- vsub
  "向量相减."
  [[ax ay az] [bx by bz]]
  [(- ax bx) (- ay by) (- az bz)])

(defn- vscale
  "向量缩放."
  [[x y z] s]
  [(* x s) (* y s) (* z s)])

(defn- vcross
  "向量叉乘."
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn- vlen
  "向量长度."
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn- gcd
  "最大公约数."
  [a b]
  (let [a (Math/abs (long a))
        b (Math/abs (long b))]
    (loop [x a y b]
      (if (zero? y) x (recur y (mod x y))))))

(defn- lcm
  "最小公倍数."
  [a b]
  (let [a (long a)
        b (long b)]
    (if (or (zero? a) (zero? b))
      0
      (Math/abs (long (/ (* a b) (gcd a b)))))))

(defn uv-sphere
  "生成 UV 球体网格, 返回 {:vertices float-array :indices int-array}.
  参数:
    lat-segs - 纬向分段数
    lon-segs - 经向分段数"
  [lat-segs lon-segs]
  (let [lat-segs (max 3 (int lat-segs))
        lon-segs (max 3 (int lon-segs))
        vertex-count (* (inc lat-segs) (inc lon-segs))
        vertices (transient [])
        indices (transient [])]
    (dotimes [i (inc lat-segs)]
      (let [v (/ (double i) lat-segs)
            phi (* Math/PI v)
            y (Math/cos phi)
            r (Math/sin phi)]
        (dotimes [j (inc lon-segs)]
          (let [u (/ (double j) lon-segs)
                theta (* 2.0 Math/PI u)
                x (* r (Math/cos theta))
                z (* r (Math/sin theta))
                nx x
                ny y
                nz z]
            (conj! vertices (float x))
            (conj! vertices (float y))
            (conj! vertices (float z))
            (conj! vertices (float nx))
            (conj! vertices (float ny))
            (conj! vertices (float nz))))))
    (dotimes [i lat-segs]
      (dotimes [j lon-segs]
        (let [row-a (* i (inc lon-segs))
              row-b (* (inc i) (inc lon-segs))
              a (+ row-a j)
              b (+ row-b j)
              c (+ row-b (inc j))
              d (+ row-a (inc j))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))
     :vertex-count vertex-count}))

(defn make-point-cloud
  "创建点云数据. 传入 points 与可选 colors.
  points 为 [[x y z] ...].
  colors 为 [[r g b] ...], 若不足则使用白色."
  [points & {:keys [colors]}]
  {:points (vec points)
   :colors (vec (or colors []))})

(defn sphere-point-cloud
  "生成球面点云."
  [count radius]
  (let [count (max 1 (int count))
        radius (double radius)]
    (vec
     (repeatedly count
                 (fn []
                   (let [u (rand)
                         v (rand)
                         theta (* 2.0 Math/PI u)
                         phi (Math/acos (- 1.0 (* 2.0 v)))
                         x (* radius (Math/sin phi) (Math/cos theta))
                         y (* radius (Math/cos phi))
                         z (* radius (Math/sin phi) (Math/sin theta))]
                     [(float x) (float y) (float z)]))))))

(defn line-points
  "生成线段上的点. num-segments 为分段数."
  [p1 p2 num-segments]
  (let [num-segments (max 1 (int num-segments))
        [x1 y1 z1] p1
        [x2 y2 z2] p2]
    (mapv (fn [i]
            (let [t (safe-frac i num-segments)]
              [(float (lerp t x1 x2))
               (float (lerp t y1 y2))
               (float (lerp t z1 z2))]))
          (range 0 (inc num-segments)))))

(defn rectangle-points
  "生成矩形轮廓点. num-segments 为每边分段数."
  [width height & [num-segments]]
  (let [num-segments (max 1 (int (or num-segments 1)))
        x (/ (double width) 2.0)
        y (/ (double height) 2.0)
        p0 [x y 0.0]
        p1 [(- x) y 0.0]
        p2 [(- x) (- y) 0.0]
        p3 [x (- y) 0.0]
        side-0 (line-points p0 p1 num-segments)
        side-1 (line-points p1 p2 num-segments)
        side-2 (line-points p2 p3 num-segments)
        side-3 (line-points p3 p0 num-segments)]
    (vec (concat (butlast side-0)
                 (butlast side-1)
                 (butlast side-2)
                 (butlast side-3)))))

(defn square-points
  "生成正方形轮廓点. num-segments 为每边分段数."
  [side & [num-segments]]
  (rectangle-points side side num-segments))

(defn circle-points
  "生成圆形轮廓点. num-segments 为分段数."
  [diameter num-segments]
  (let [num-segments (max 3 (int num-segments))
        radius (/ (double diameter) 2.0)
        angle-delta (/ tau num-segments)
        points (mapv (fn [i]
                       (let [angle (* i angle-delta)]
                         [(float (* (Math/sin angle) radius))
                          (float (* (Math/cos angle) radius))
                          0.0]))
                     (range 0 num-segments))]
    (vec (reverse points))))

(defn arc-points
  "生成圆弧点. start-angle/end-angle 为角度制, num-segments 为分段数."
  [diameter start-angle end-angle num-segments]
  (let [num-segments (max 1 (int num-segments))
        radius (/ (double diameter) 2.0)
        angle-delta (/ (- (deg->rad end-angle) (deg->rad start-angle)) num-segments)]
    (mapv (fn [i]
            (let [angle (+ (* i angle-delta) (deg->rad start-angle))]
              [(float (* (Math/sin angle) radius))
               (float (* (Math/cos angle) radius))
               0.0]))
          (range 0 (inc num-segments)))))

(defn spiral-points
  "生成螺旋曲线点. axis-length 为 Z 轴长度, num-loops 为圈数."
  [start-diameter end-diameter axis-length num-loops num-segments]
  (let [num-segments (max 1 (int num-segments))
        start-radius (/ (double start-diameter) 2.0)
        end-radius (/ (double end-diameter) 2.0)
        angle-delta (/ (* tau num-loops) num-segments)
        len-delta (/ (double axis-length) num-segments)
        points (mapv (fn [i]
                       (let [angle (* i angle-delta)
                             len (* i len-delta)
                             t (safe-frac i num-segments)
                             radius (lerp t start-radius end-radius)]
                         [(float (* (Math/sin angle) radius))
                          (float (* (Math/cos angle) radius))
                          (float len)]))
                     (range 0 (inc num-segments)))]
    (vec (reverse points))))

(defn sine-curve-points
  "生成正弦曲线点. period 为角度周期."
  [period frequency x-scale y-scale num-segments]
  (let [num-segments (max 1 (int num-segments))
        rad-period (deg->rad period)
        angle-delta (/ rad-period num-segments)]
    (mapv (fn [i]
            (let [angle (* i angle-delta frequency)
                  x (* x-scale (/ angle (* frequency rad-period)))
                  y (* y-scale (Math/sin angle))]
              [(float x) (float y) 0.0]))
          (range 0 (inc num-segments)))))

(defn random-points
  "生成随机点云. bounds-lo/bounds-hi 为包围盒角点."
  [num bounds-lo bounds-hi]
  (let [[lx ly lz] bounds-lo
        [hx hy hz] bounds-hi
        num (max 1 (int num))]
    (vec
     (repeatedly num
                 (fn []
                   [(float (math/rand-range lx hx))
                    (float (math/rand-range ly hy))
                    (float (math/rand-range lz hz))])))))

(defn grid-points
  "生成规则网格点云. nx/ny/nz 为各方向采样数."
  [nx ny nz bounds-lo bounds-hi]
  (let [nx (max 1 (int nx))
        ny (max 1 (int ny))
        nz (max 1 (int nz))
        [lx ly lz] bounds-lo
        [hx hy hz] bounds-hi]
    (vec
     (for [ix (range nx)
           iy (range ny)
           iz (range nz)]
       (let [fx (if (> nx 1) (/ ix (double (dec nx))) 0.0)
             fy (if (> ny 1) (/ iy (double (dec ny))) 0.0)
             fz (if (> nz 1) (/ iz (double (dec nz))) 0.0)]
         [(float (lerp fx lx hx))
          (float (lerp fy ly hy))
          (float (lerp fz lz hz))])))))

(defn heightfield-height-fn
  "生成高度场高度函数."
  [& {:keys [amp noise-scale octaves lacunarity gain]
      :or {amp 0.35 noise-scale 0.6 octaves 4 lacunarity 2.0 gain 0.5}}]
  (let [amp (double amp)
        noise-scale (double noise-scale)
        octaves (int octaves)
        lacunarity (double lacunarity)
        gain (double gain)]
    (fn [x z]
      (* amp
         (fbm2 (* x noise-scale)
               (* z noise-scale)
               octaves lacunarity gain)))))

(defn heightfield-points
  "生成高度场表面点云."
  [width depth seg-x seg-z & {:keys [height-fn amp noise-scale octaves lacunarity gain]
                              :or {amp 0.35 noise-scale 0.6 octaves 4 lacunarity 2.0 gain 0.5}}]
  (let [seg-x (max 1 (int seg-x))
        seg-z (max 1 (int seg-z))
        half-w (/ (double width) 2.0)
        half-d (/ (double depth) 2.0)
        height-fn (or height-fn
                      (heightfield-height-fn
                       :amp amp
                       :noise-scale noise-scale
                       :octaves octaves
                       :lacunarity lacunarity
                       :gain gain))]
    (vec
     (for [iz (range (inc seg-z))
           ix (range (inc seg-x))]
       (let [fx (/ (double ix) seg-x)
             fz (/ (double iz) seg-z)
             x (lerp fx (- half-w) half-w)
             z (lerp fz (- half-d) half-d)
             y (height-fn x z)]
         [(float x) (float y) (float z)])))))

(defn heightfield-mesh
  "生成高度场网格, 返回 {:vertices :indices :heights}."
  [width depth seg-x seg-z & {:keys [height-fn amp noise-scale octaves lacunarity gain]
                              :or {amp 0.35 noise-scale 0.6 octaves 4 lacunarity 2.0 gain 0.5}}]
  (let [seg-x (max 1 (int seg-x))
        seg-z (max 1 (int seg-z))
        nx (inc seg-x)
        nz (inc seg-z)
        half-w (/ (double width) 2.0)
        half-d (/ (double depth) 2.0)
        height-fn (or height-fn
                      (heightfield-height-fn
                       :amp amp
                       :noise-scale noise-scale
                       :octaves octaves
                       :lacunarity lacunarity
                       :gain gain))
        heights (vec
                 (for [iz (range nz)
                       ix (range nx)]
                   (let [fx (/ (double ix) seg-x)
                         fz (/ (double iz) seg-z)
                         x (lerp fx (- half-w) half-w)
                         z (lerp fz (- half-d) half-d)]
                     (height-fn x z))))
        height-at (fn [ix iz]
                    (let [ix (min (max ix 0) (dec nx))
                          iz (min (max iz 0) (dec nz))]
                      (nth heights (+ ix (* iz nx)))))
        vertices (transient [])
        indices (transient [])]
    (dotimes [iz nz]
      (dotimes [ix nx]
        (let [fx (/ (double ix) seg-x)
              fz (/ (double iz) seg-z)
              x (lerp fx (- half-w) half-w)
              z (lerp fz (- half-d) half-d)
              y (height-at ix iz)
              hL (height-at (dec ix) iz)
              hR (height-at (inc ix) iz)
              hD (height-at ix (dec iz))
              hU (height-at ix (inc iz))
              nxv (- hL hR)
              nzv (- hD hU)
              [nxv nyv nzv] (vnormalize [nxv 2.0 nzv])]
          (conj! vertices (float x))
          (conj! vertices (float y))
          (conj! vertices (float z))
          (conj! vertices (float nxv))
          (conj! vertices (float nyv))
          (conj! vertices (float nzv)))))
    (dotimes [iz seg-z]
      (dotimes [ix seg-x]
        (let [row-a (* iz nx)
              row-b (* (inc iz) nx)
              a (+ row-a ix)
              b (+ row-b ix)
              c (+ row-b (inc ix))
              d (+ row-a (inc ix))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))
     :heights heights}))

(defn bezier-curve-points
  "生成三次贝塞尔曲线点."
  [cv0 cv1 cv2 cv3 num-segments]
  (let [num-segments (max 1 (int num-segments))
        [x0 y0 z0] cv0
        [x1 y1 z1] cv1
        [x2 y2 z2] cv2
        [x3 y3 z3] cv3]
    (mapv
     (fn [i]
       (let [t (safe-frac i num-segments)
             u (- 1.0 t)
             uu (* u u)
             uuu (* uu u)
             tt (* t t)
             ttt (* tt t)
             x (+ (* uuu x0)
                  (* 3.0 uu t x1)
                  (* 3.0 u tt x2)
                  (* ttt x3))
             y (+ (* uuu y0)
                  (* 3.0 uu t y1)
                  (* 3.0 u tt y2)
                  (* ttt y3))
             z (+ (* uuu z0)
                  (* 3.0 uu t z1)
                  (* 3.0 u tt z2)
                  (* ttt z3))]
         [(float x) (float y) (float z)]))
     (range 0 (inc num-segments)))))

(defn butterfly-curve-points
  "生成蝴蝶曲线点."
  [num-segments]
  (let [num-segments (max 4 (int num-segments))
        angle-delta (/ (* 12.0 Math/PI) num-segments)]
    (mapv
     (fn [i]
       (let [angle (* i angle-delta)
             radius (- (Math/pow Math/E (Math/cos angle))
                       (* 2.0 (Math/cos (* 4.0 angle)))
                       (Math/pow (Math/sin (/ angle 12.0)) 5.0))]
         [(float (* (Math/sin angle) radius))
          (float (* (Math/cos angle) radius))
          0.0]))
     (range 0 num-segments))))

(defn hypotrochoid-curve-points
  "生成内旋轮线曲线点."
  [fixed-r rolling-r dist num-segments]
  (let [num-segments (max 8 (int num-segments))
        fixed-r (long fixed-r)
        rolling-r (long rolling-r)
        total-sweep (* tau (/ (lcm fixed-r rolling-r) (double fixed-r)))
        theta-delta (/ total-sweep num-segments)]
    (mapv
     (fn [i]
       (let [theta (* i theta-delta)
             angle (* (/ (- fixed-r rolling-r) (double rolling-r)) theta)
             x (+ (* (- fixed-r rolling-r) (Math/cos theta))
                  (* dist (Math/cos angle)))
             y (- (* (- fixed-r rolling-r) (Math/sin theta))
                  (* dist (Math/sin angle)))]
         [(float (/ x fixed-r)) (float (/ y fixed-r)) 0.0]))
     (range 0 num-segments))))

(defn epitrochoid-curve-points
  "生成外旋轮线曲线点."
  [fixed-r rolling-r dist num-segments]
  (let [num-segments (max 8 (int num-segments))
        fixed-r (long fixed-r)
        rolling-r (long rolling-r)
        total-sweep (* tau (/ (lcm fixed-r rolling-r) (double fixed-r)))
        theta-delta (/ total-sweep num-segments)]
    (mapv
     (fn [i]
       (let [theta (* i theta-delta)
             angle (* (/ (+ fixed-r rolling-r) (double rolling-r)) theta)
             x (- (* (+ fixed-r rolling-r) (Math/cos theta))
                  (* dist (Math/cos angle)))
             y (- (* (+ fixed-r rolling-r) (Math/sin theta))
                  (* dist (Math/sin angle)))]
         [(float (/ x fixed-r)) (float (/ y fixed-r)) 0.0]))
     (range 0 num-segments))))

;; ---- 多面体与网格 ---------------------------------------------------------

(defn- vsub
  [a b]
  [(- (double (nth a 0)) (double (nth b 0)))
   (- (double (nth a 1)) (double (nth b 1)))
   (- (double (nth a 2)) (double (nth b 2)))])

(defn- vcross
  [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn- vlength
  [[x y z]]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

(defn- vnormalize
  [v]
  (let [len (vlength v)]
    (if (<= len 1.0e-9)
      [0.0 0.0 0.0]
      [(/ (nth v 0) len) (/ (nth v 1) len) (/ (nth v 2) len)])))

(defn- face->triangles
  [face]
  (let [n (count face)]
    (cond
      (< n 3) []
      (= n 3) [(vec face)]
      :else (mapv (fn [i] [(nth face 0) (nth face i) (nth face (inc i))])
                  (range 1 (- n 1))))))

(defn- polyhedron->mesh
  "将多面体顶点/面转为三角形网格."
  [points faces]
  (let [triangles (mapcat face->triangles faces)
        verts (transient [])
        indices (transient [])
        idx (atom 0)]
    (doseq [[i0 i1 i2] triangles]
      (let [p0 (nth points i0)
            p1 (nth points i1)
            p2 (nth points i2)
            n (vnormalize (vcross (vsub p1 p0) (vsub p2 p0)))]
        (doseq [p [p0 p1 p2]]
          (conj! verts (float (nth p 0)))
          (conj! verts (float (nth p 1)))
          (conj! verts (float (nth p 2)))
          (conj! verts (float (nth n 0)))
          (conj! verts (float (nth n 1)))
          (conj! verts (float (nth n 2)))
          (conj! indices @idx)
          (swap! idx inc))))
    {:vertices (float-array (persistent! verts))
     :indices (int-array (persistent! indices))}))

(defn tetrahedron-mesh
  "生成四面体网格."
  [diameter]
  (let [r (* (double diameter) 0.5)
        -r (* (double diameter) -0.5)
        points [[r (/ -r (Math/sqrt 6.0)) (/ -r (Math/sqrt 3.0))]
                [-r (/ -r (Math/sqrt 6.0)) (/ -r (Math/sqrt 3.0))]
                [0.0 (/ -r (Math/sqrt 6.0)) (/ (* 2.0 r) (Math/sqrt 3.0))]
                [0.0 (/ (* 3.0 r) (Math/sqrt 6.0)) 0.0]]
        faces [[0 2 1] [0 3 2] [1 2 3] [0 1 3]]]
    (polyhedron->mesh points faces)))

(defn cube-mesh
  "生成立方体网格."
  [side]
  (let [r (* (double side) 0.5)
        -r (* (double side) -0.5)
        points [[-r -r -r]
                [r -r -r]
                [r -r r]
                [-r -r r]
                [-r r -r]
                [r r -r]
                [r r r]
                [-r r r]]
        faces [[0 1 2 3] [0 4 5 1] [1 5 6 2]
               [2 6 7 3] [3 7 4 0] [4 7 6 5]]]
    (polyhedron->mesh points faces)))

(defn octahedron-mesh
  "生成八面体网格."
  [diameter]
  (let [r (Math/abs (/ (double diameter) 2.0))
        -r (- r)
        points [[r 0.0 0.0]
                [-r 0.0 0.0]
                [0.0 r 0.0]
                [0.0 -r 0.0]
                [0.0 0.0 r]
                [0.0 0.0 -r]]
        faces [[0 2 4] [2 0 5] [3 0 4] [0 3 5]
               [2 1 4] [1 2 5] [1 3 4] [3 1 5]]]
    (polyhedron->mesh points faces)))

(defn dodecahedron-mesh
  "生成十二面体网格."
  [diameter]
  (let [r (/ (double diameter) 4.0)
        phi (* 1.61803 r)
        inv (* 0.6180355 r)
        points [[0.0 inv phi]
                [0.0 (- inv) phi]
                [0.0 (- inv) (- phi)]
                [0.0 inv (- phi)]
                [phi 0.0 inv]
                [(- phi) 0.0 inv]
                [(- phi) 0.0 (- inv)]
                [phi 0.0 (- inv)]
                [inv phi 0.0]
                [(- inv) phi 0.0]
                [(- inv) (- phi) 0.0]
                [inv (- phi) 0.0]
                [r r r]
                [(- r) r r]
                [(- r) (- r) r]
                [r (- r) r]
                [r (- r) (- r)]
                [r r (- r)]
                [(- r) r (- r)]
                [(- r) (- r) (- r)]]
        faces [[0 1 15 4 12]
               [0 12 8 9 13]
               [0 13 5 14 1]
               [1 14 10 11 15]
               [2 3 17 7 16]
               [2 16 11 10 19]
               [2 19 6 18 3]
               [18 9 8 17 3]
               [15 11 16 7 4]
               [4 7 17 8 12]
               [13 9 18 6 5]
               [5 6 19 10 14]]]
    (polyhedron->mesh points faces)))

(defn icosahedron-mesh
  "生成二十面体网格."
  [diameter]
  (let [p1 (/ (Math/abs (/ (double diameter) 2.0)) 1.902076)
        p2 (* p1 1.618034)
        -p1 (- p1)
        -p2 (- p2)
        points [[p2 p1 0.0]
                [-p2 p1 0.0]
                [p2 -p1 0.0]
                [-p2 -p1 0.0]
                [p1 0.0 p2]
                [p1 0.0 -p2]
                [-p1 0.0 p2]
                [-p1 0.0 -p2]
                [0.0 p2 p1]
                [0.0 -p2 p1]
                [0.0 p2 -p1]
                [0.0 -p2 -p1]]
        faces [[0 8 4] [0 5 10] [2 4 9] [2 11 5] [1 6 8] [1 10 7]
               [3 9 6] [3 7 11] [0 10 8] [1 8 10] [2 9 11]
               [3 11 9] [4 2 0] [5 0 2] [6 1 3] [7 3 1] [8 6 4]
               [9 4 6] [10 5 7] [11 7 5]]]
    (polyhedron->mesh points faces)))

;; ---- 多面体细分/分型 -------------------------------------------------------

(defn tetrahedron-polyhedron
  "生成四面体多面体数据."
  [diameter]
  (let [r (* (double diameter) 0.5)
        -r (* (double diameter) -0.5)]
    {:points [[r (/ -r (Math/sqrt 6.0)) (/ -r (Math/sqrt 3.0))]
              [-r (/ -r (Math/sqrt 6.0)) (/ -r (Math/sqrt 3.0))]
              [0.0 (/ -r (Math/sqrt 6.0)) (/ (* 2.0 r) (Math/sqrt 3.0))]
              [0.0 (/ (* 3.0 r) (Math/sqrt 6.0)) 0.0]]
     :faces [[0 2 1] [0 3 2] [1 2 3] [0 1 3]]}))

(defn cube-polyhedron
  "生成立方体多面体数据."
  [side]
  (let [r (* (double side) 0.5)
        -r (* (double side) -0.5)]
    {:points [[-r -r -r]
              [r -r -r]
              [r -r r]
              [-r -r r]
              [-r r -r]
              [r r -r]
              [r r r]
              [-r r r]]
     :faces [[0 1 2 3] [0 4 5 1] [1 5 6 2]
             [2 6 7 3] [3 7 4 0] [4 7 6 5]]}))

(defn octahedron-polyhedron
  "生成八面体多面体数据."
  [diameter]
  (let [r (Math/abs (/ (double diameter) 2.0))
        -r (- r)]
    {:points [[r 0.0 0.0]
              [-r 0.0 0.0]
              [0.0 r 0.0]
              [0.0 -r 0.0]
              [0.0 0.0 r]
              [0.0 0.0 -r]]
     :faces [[0 2 4] [2 0 5] [3 0 4] [0 3 5]
             [2 1 4] [1 2 5] [1 3 4] [3 1 5]]}))

(defn dodecahedron-polyhedron
  "生成十二面体多面体数据."
  [diameter]
  (let [r (/ (double diameter) 4.0)
        phi (* 1.61803 r)
        inv (* 0.6180355 r)]
    {:points [[0.0 inv phi]
              [0.0 (- inv) phi]
              [0.0 (- inv) (- phi)]
              [0.0 inv (- phi)]
              [phi 0.0 inv]
              [(- phi) 0.0 inv]
              [(- phi) 0.0 (- inv)]
              [phi 0.0 (- inv)]
              [inv phi 0.0]
              [(- inv) phi 0.0]
              [(- inv) (- phi) 0.0]
              [inv (- phi) 0.0]
              [r r r]
              [(- r) r r]
              [(- r) (- r) r]
              [r (- r) r]
              [r (- r) (- r)]
              [r r (- r)]
              [(- r) r (- r)]
              [(- r) (- r) (- r)]]
     :faces [[0 1 15 4 12]
             [0 12 8 9 13]
             [0 13 5 14 1]
             [1 14 10 11 15]
             [2 3 17 7 16]
             [2 16 11 10 19]
             [2 19 6 18 3]
             [18 9 8 17 3]
             [15 11 16 7 4]
             [4 7 17 8 12]
             [13 9 18 6 5]
             [5 6 19 10 14]]}))

(defn icosahedron-polyhedron
  "生成二十面体多面体数据."
  [diameter]
  (let [p1 (/ (Math/abs (/ (double diameter) 2.0)) 1.902076)
        p2 (* p1 1.618034)
        -p1 (- p1)
        -p2 (- p2)]
    {:points [[p2 p1 0.0]
              [-p2 p1 0.0]
              [p2 -p1 0.0]
              [-p2 -p1 0.0]
              [p1 0.0 p2]
              [p1 0.0 -p2]
              [-p1 0.0 p2]
              [-p1 0.0 -p2]
              [0.0 p2 p1]
              [0.0 -p2 p1]
              [0.0 p2 -p1]
              [0.0 -p2 -p1]]
     :faces [[0 8 4] [0 5 10] [2 4 9] [2 11 5] [1 6 8] [1 10 7]
             [3 9 6] [3 7 11] [0 10 8] [1 8 10] [2 9 11]
             [3 11 9] [4 2 0] [5 0 2] [6 1 3] [7 3 1] [8 6 4]
             [9 4 6] [10 5 7] [11 7 5]]}))

(defn polyhedron-mesh
  "将多面体数据转为网格."
  [{:keys [points faces]}]
  (polyhedron->mesh points faces))

(defn mesh-from-faces
  "将顶点/面数据转换为可渲染的三角形网格。
  返回 {:vertices float-array :indices int-array}，法线按面计算。"
  [vertices faces]
  (polyhedron-mesh {:points vertices :faces faces}))

(defn- face-normal
  [points face]
  (if (< (count face) 3)
    [0.0 0.0 0.0]
    (let [p0 (nth points (nth face 0))
          p1 (nth points (nth face 1))
          p2 (nth points (nth face 2))]
      (vnormalize (vcross (vsub p1 p0) (vsub p2 p0))))))

(defn- avg-points
  [pts]
  (let [n (count pts)]
    (if (zero? n)
      [0.0 0.0 0.0]
      (mapv #(/ % (double n))
            (reduce (fn [acc p] (mapv + acc p)) [0.0 0.0 0.0] pts)))))

(defn polyhedron-vertices
  "返回多面体顶点列表."
  [poly]
  (mapv (fn [[x y z]] [(float x) (float y) (float z)]) (:points poly)))

(defn polyhedron-face-centers
  "返回多面体面中心点列表."
  [poly]
  (let [points (:points poly)]
    (mapv (fn [face]
            (avg-points (mapv points face)))
          (:faces poly))))

(defn- edge-key
  [a b]
  (if (< a b) [a b] [b a]))

(defn- build-edge-map
  [faces]
  (reduce-kv
   (fn [m fi face]
     (let [cnt (count face)]
       (loop [i 0 m m]
         (if (= i cnt)
           m
           (let [a (nth face i)
                 b (nth face (mod (inc i) cnt))
                 key (edge-key a b)]
             (recur (inc i)
                    (update m key
                            (fnil (fn [e] (update e :faces conj fi))
                                  {:v0 (first key) :v1 (second key) :faces []}))))))))
   {}
   (vec faces)))

(defn- build-vertex-faces
  [faces]
  (reduce-kv
   (fn [m fi face]
     (reduce (fn [m v] (update m v (fnil conj []) fi)) m face))
   {}
   (vec faces)))

(defn- build-vertex-edges
  [edge-map]
  (reduce
   (fn [m [k {:keys [v0 v1]}]]
     (-> m
         (update v0 (fnil conj []) k)
         (update v1 (fnil conj []) k)))
   {}
   edge-map))

(defn- compute-vertex-normals
  [points faces]
  (let [normals (vec (repeat (count points) [0.0 0.0 0.0]))]
    (let [normals
          (reduce
           (fn [acc face]
             (let [n (face-normal points face)]
               (reduce (fn [acc v]
                         (update acc v (fn [curr] (mapv + curr n))))
                       acc
                       face)))
           normals
           faces)]
      (mapv vnormalize normals))))

(defn- subdivide-polyhedron
  [poly mode]
  (let [{:keys [points faces]} poly
        points (vec points)
        faces (vec faces)
        face-centers (mapv (fn [face] (avg-points (mapv #(nth points %) face))) faces)
        edge-map (build-edge-map faces)
        edge-keys (vec (sort-by (fn [[a b]] [a b]) (keys edge-map)))
        edge-midpoints (into {}
                             (map (fn [k]
                                    (let [{:keys [v0 v1]} (get edge-map k)
                                          p0 (nth points v0)
                                          p1 (nth points v1)]
                                      [k (mapv #(/ % 2.0) (mapv + p0 p1))]))
                                  edge-keys))
        edge-points (into {}
                          (map (fn [k]
                                 (let [{:keys [v0 v1 faces]} (get edge-map k)
                                       p0 (nth points v0)
                                       p1 (nth points v1)
                                       mid (get edge-midpoints k)
                                       p (if (and (= mode :smooth) (= 2 (count faces)))
                                           (let [f0 (nth face-centers (first faces))
                                                 f1 (nth face-centers (second faces))]
                                             (mapv #(/ % 4.0) (mapv + p0 p1 f0 f1)))
                                           mid)]
                                   [k p]))
                               edge-keys))
        vertex-faces (build-vertex-faces faces)
        vertex-edges (build-vertex-edges edge-map)
        vertex-points
        (mapv
         (fn [idx]
           (let [p (nth points idx)]
             (if (= mode :smooth)
               (let [faces* (get vertex-faces idx [])
                     edges* (get vertex-edges idx [])
                     n (count faces*)]
                 (if (zero? n)
                   p
                   (let [f (avg-points (mapv #(nth face-centers %) faces*))
                         r (avg-points (mapv #(get edge-midpoints %) edges*))
                         [px py pz] p
                         [fx fy fz] f
                         [rx ry rz] r
                         nn (double n)]
                     [(/ (+ fx (* 2.0 rx) (* (- nn 3.0) px)) nn)
                      (/ (+ fy (* 2.0 ry) (* (- nn 3.0) py)) nn)
                      (/ (+ fz (* 2.0 rz) (* (- nn 3.0) pz)) nn)])))
               p)))
         (range (count points)))
        v-count (count vertex-points)
        edge-index (zipmap edge-keys (range v-count (+ v-count (count edge-keys))))
        face-index (zipmap (range (count faces))
                           (range (+ v-count (count edge-keys))
                                  (+ v-count (count edge-keys) (count faces))))
        new-faces
        (vec
         (mapcat
          (fn [fi face]
            (let [cnt (count face)
                  f-idx (get face-index fi)]
              (mapv (fn [i]
                      (let [v (nth face i)
                            v-next (nth face (mod (inc i) cnt))
                            v-prev (nth face (mod (dec i) cnt))
                            e-next (get edge-index (edge-key v v-next))
                            e-prev (get edge-index (edge-key v-prev v))]
                        [v e-next f-idx e-prev]))
                    (range cnt))))
          (range (count faces))
          faces))
        new-points (vec (concat vertex-points
                                (mapv edge-points edge-keys)
                                face-centers))]
    {:points new-points :faces new-faces}))

(defn refine-polyhedron
  "细分多面体。levels 为细分层数."
  [poly levels]
  (loop [poly poly
         levels (int levels)]
    (if (<= levels 0)
      poly
      (recur (subdivide-polyhedron poly :refine)
             (dec levels)))))

(defn smooth-polyhedron
  "平滑细分多面体。levels 为细分层数."
  [poly levels]
  (loop [poly poly
         levels (int levels)]
    (if (<= levels 0)
      poly
      (recur (subdivide-polyhedron poly :smooth)
             (dec levels)))))

(defn fractalize-polyhedron
  "分型细分多面体。displacement 控制扰动幅度."
  [poly levels displacement]
  (loop [poly poly
         levels (int levels)
         disp (double displacement)]
    (if (<= levels 0)
      poly
      (let [orig-count (count (:points poly))
            poly (subdivide-polyhedron poly :refine)
            normals (compute-vertex-normals (:points poly) (:faces poly))
            points (mapv (fn [idx p]
                           (if (< idx orig-count)
                             p
                             (let [n (nth normals idx)
                                   d (math/rand-range (- disp) disp)]
                               [(float (+ (nth p 0) (* (nth n 0) d)))
                                (float (+ (nth p 1) (* (nth n 1) d)))
                                (float (+ (nth p 2) (* (nth n 2) d)))])))
                         (range (count (:points poly)))
                         (:points poly))]
        (recur {:points points :faces (:faces poly)}
               (dec levels)
               (/ disp 2.0))))))

(defn box-polyhedron
  "生成盒子多面体数据。"
  [x-size y-size z-size]
  (let [x (* (double x-size) 0.5)
        y (* (double y-size) 0.5)
        z (* (double z-size) 0.5)]
    {:points [[(- x) (- y) (- z)]
              [x (- y) (- z)]
              [x (- y) z]
              [(- x) (- y) z]
              [(- x) y (- z)]
              [x y (- z)]
              [x y z]
              [(- x) y z]]
     :faces [[0 1 2 3] [0 4 5 1] [1 5 6 2]
             [2 6 7 3] [3 7 4 0] [4 7 6 5]]}))

(defn cut-cube-polyhedron
  "生成切角立方体多面体数据。"
  [side]
  (let [r (* (double side) 0.5)
        -r (* (double side) -0.5)
        b (* (double side) 0.3)]
    {:points [[-r -r -r]
              [r -r -r]
              [r -r r]
              [-r -r r]
              [-r r -r]
              [r r -r]
              [r r b]
              [b r r]
              [-r r r]
              [r b r]]
     :faces [[1 2 3 0]
             [5 6 9 2 1]
             [9 7 8 3 2]
             [0 4 5 1]
             [8 4 0 3]
             [8 7 6 5 4]
             [6 7 9]]}))

(defn sweep-mesh
  "生成沿 X 轴扫掠的管状网格. 可通过 :taper 自定义截面缩放."
  [& {:keys [segments ring length radius amp freq twist taper-override]
      :or {segments 80 ring 16 length 3.0 radius 0.25 amp 0.35 freq 1.5 twist 0.0}}]
  (let [segments (max 3 (int segments))
        ring (max 6 (int ring))
        length (double length)
        radius (double radius)
        amp (double amp)
        freq (double freq)
        twist (Math/toRadians (double twist))
        vertices (transient [])
        indices (transient [])]
    (dotimes [i (inc segments)]
      (let [t (/ (double i) segments)
            x (- (* t length) (* 0.5 length))
            phase (* 2.0 Math/PI freq t)
            y (* amp (Math/sin phase))
            z (* (* 0.5 amp) (Math/cos phase))
            taper (if (nil? taper-override)
                    (+ 0.85 (* 0.25 (Math/sin (* 2.0 Math/PI t))))
                    (double taper-override))
            r (* radius taper)
            twist-angle (* twist t)]
        (dotimes [j (inc ring)]
          (let [u (/ (double j) ring)
                theta (+ (* 2.0 Math/PI u) twist-angle)
                cy (Math/cos theta)
                sy (Math/sin theta)
                px x
                py (+ y (* r cy))
                pz (+ z (* r sy))
                nx 0.0
                ny cy
                nz sy]
            (conj! vertices (float px))
            (conj! vertices (float py))
            (conj! vertices (float pz))
            (conj! vertices (float nx))
            (conj! vertices (float ny))
            (conj! vertices (float nz))))))
    (dotimes [i segments]
      (dotimes [j ring]
        (let [row-a (* i (inc ring))
              row-b (* (inc i) (inc ring))
              a (+ row-a j)
              b (+ row-b j)
              c (+ row-b (inc j))
              d (+ row-a (inc j))]
          (conj! indices a)
          (conj! indices b)
          (conj! indices d)
          (conj! indices b)
          (conj! indices c)
          (conj! indices d))))
    {:vertices (float-array (persistent! vertices))
     :indices (int-array (persistent! indices))}))

(defn- tube-mesh-data
  "根据路径生成管状网格数据."
  [points ring radius]
  (let [points (vec points)
        n (count points)
        ring (max 3 (int ring))
        radius (double radius)
        vertices (transient [])
        indices (transient [])]
    (when (>= n 2)
      (dotimes [i n]
        (let [p (nth points i)
              p-prev (nth points (max 0 (dec i)))
              p-next (nth points (min (dec n) (inc i)))
              forward (vnormalize (vsub p-next p-prev))
              up0 [0.0 1.0 0.0]
              right0 (vcross forward up0)
              right0 (if (<= (vlen right0) 1.0e-6)
                       (vcross forward [1.0 0.0 0.0])
                       right0)
              right (vnormalize right0)
              up (vnormalize (vcross right forward))
              [px py pz] p]
          (dotimes [j (inc ring)]
            (let [u (/ (double j) ring)
                  theta (* 2.0 Math/PI u)
                  cy (Math/cos theta)
                  sy (Math/sin theta)
                  offset (vadd (vscale right cy) (vscale up sy))
                  [ox oy oz] offset
                  nxv ox
                  nyv oy
                  nzv oz
                  vx (+ px (* radius ox))
                  vy (+ py (* radius oy))
                  vz (+ pz (* radius oz))]
              (conj! vertices (float vx))
              (conj! vertices (float vy))
              (conj! vertices (float vz))
              (conj! vertices (float nxv))
              (conj! vertices (float nyv))
              (conj! vertices (float nzv))))))
      (dotimes [i (dec n)]
        (dotimes [j ring]
          (let [row-a (* i (inc ring))
                row-b (* (inc i) (inc ring))
                a (+ row-a j)
                b (+ row-b j)
                c (+ row-b (inc j))
                d (+ row-a (inc j))]
            (conj! indices a)
            (conj! indices b)
            (conj! indices d)
            (conj! indices b)
            (conj! indices c)
            (conj! indices d)))))
    {:vertices (persistent! vertices)
     :indices (persistent! indices)
     :vertex-count (* n (inc ring))}))

(defn tube-mesh
  "根据路径生成管状网格."
  [points & {:keys [ring radius] :or {ring 10 radius 0.08}}]
  (let [{:keys [vertices indices]} (tube-mesh-data points ring radius)]
    {:vertices (float-array vertices)
     :indices (int-array indices)}))

(defn tube-meshes
  "根据多条路径生成合并的管状网格."
  [paths & {:keys [ring radius] :or {ring 10 radius 0.08}}]
  (let [verts (transient [])
        inds (transient [])
        offset (atom 0)]
    (doseq [path paths]
      (let [{:keys [vertices indices vertex-count]} (tube-mesh-data path ring radius)
            base @offset]
        (when (seq indices)
          (doseq [v vertices] (conj! verts (float v)))
          (doseq [i indices] (conj! inds (+ base i)))
          (swap! offset + vertex-count))))
    {:vertices (float-array (persistent! verts))
     :indices (int-array (persistent! inds))}))
