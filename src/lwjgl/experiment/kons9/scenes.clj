(ns lwjgl.experiment.kons9.scenes
  (:require [lwjgl.experiment.kons9.geometry :as kgeom]
            [lwjgl.experiment.kons9.math :as kmath]
            [lwjgl.experiment.kons9.particles :as kpart]
            [lwjgl.experiment.kons9.sdf :as ksdf]
            [lwjgl.experiment.kons9.spring :as kspring]))

(defn- dist2
  "计算两个点之间的平方距离。"
  [a b]
  (let [[ax ay az] a
        [bx by bz] b
        dx (- bx ax)
        dy (- by ay)
        dz (- bz az)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- dist2-2d
  "计算二维点的平方距离。"
  [[ax az] [bx bz]]
  (let [dx (- bx ax)
        dz (- bz az)]
    (+ (* dx dx) (* dz dz))))

(defn- seg-dist2-2d
  "点到线段距离平方（XZ 平面）。"
  [[px pz] [ax az] [bx bz]]
  (let [abx (- bx ax)
        abz (- bz az)
        apx (- px ax)
        apz (- pz az)
        ab2 (+ (* abx abx) (* abz abz) 1.0e-9)
        t (kmath/clamp01 (/ (+ (* apx abx) (* apz abz)) ab2))
        cx (+ ax (* t abx))
        cz (+ az (* t abz))
        dx (- px cx)
        dz (- pz cz)]
    (+ (* dx dx) (* dz dz))))

(defn- dist-to-polyline
  "点到折线的最小距离（XZ 平面）。"
  [pt curve]
  (let [segs (partition 2 1 curve)]
    (Math/sqrt (reduce (fn [m [a b]]
                         (min m (seg-dist2-2d pt a b)))
                       Double/POSITIVE_INFINITY
                       segs))))

(defn- polyline->line-data
  "折线转线段数据。"
  [points color]
  (let [[r g b] (->> color
                     flatten
                     (map float)
                     (take 3))]
    (vec
     (mapcat (fn [[a b]]
               (let [[ax ay az] a
                     [bx by bz] b]
                 [(float ax) (float ay) (float az)
                  r g b
                  (float bx) (float by) (float bz)
                  r g b]))
             (partition 2 1 points)))))

(defn- steer
  "朝目标产生速度向量。"
  [pos target speed]
  (let [[px py pz] pos
        [tx ty tz] target
        dx (- tx px)
        dy (- ty py)
        dz (- tz pz)
        dist (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz) 1.0e-6))
        s (min speed (/ speed dist))]
    [(* dx s) (* dy s) (* dz s)]))

(defn default-scenes
  "构建默认演示场景集合。api 需要提供绘制与资源更新函数。"
  [api]
  (let [{:keys [set-clear-color!
                set-cubes!
                update-cubes!
                next-cube-id!
                set-point-cloud!
                clear-point-cloud!
                set-sprites!
                clear-sprites!
                set-sprite-style!
                upload-point-cloud!
                set-mesh!
                clear-mesh!
                set-mesh-style!
                set-wireframe-overlay!
                set-mesh-index-count!
                upload-spring-lines!
                clear-spring-lines!
                set-line-segments!
                clear-line-segments!
                set-rig-segments!
                clear-rig!
                default-render]} api]
    {:baseline
     {:init (fn [_] nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :geometry
     {:init (fn [_]
              (set-clear-color! [0.05 0.05 0.08 1.0])
              (set-cubes! [{:id (next-cube-id!)
                            :pos [-1.0 0.0 0.0]
                            :rot [0.0 0.0 0.0]
                            :scale [0.6 0.6 0.6]
                            :color [0.9 0.4 0.2]}
                           {:id (next-cube-id!)
                            :pos [0.0 0.0 0.0]
                            :rot [0.0 0.0 0.0]
                            :scale [0.8 0.8 0.8]
                            :color [0.2 0.8 0.4]}
                           {:id (next-cube-id!)
                            :pos [0.8 -0.6 0.3]
                            :rot [0.0 0.0 0.0]
                            :scale [0.5 0.5 0.5]
                            :color [0.2 0.6 0.9]}])
              (let [points (kgeom/sphere-point-cloud 120 0.7)
                    colors (repeatedly (count points) #(kmath/color3 (rand) (rand) (rand)))]
                (set-point-cloud! points :colors colors))
              (let [{:keys [vertices indices]} (kgeom/uv-sphere 24 36)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      deg (* 30.0 dt)]
                  (update-cubes!
                   (fn [cs]
                     (mapv (fn [c]
                             (update c :rot (fn [[rx ry rz]]
                                              [(+ rx deg) (+ ry (* 0.6 deg)) rz])))
                           cs))))
                scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :rig
     {:init (fn [_]
              (set-clear-color! [0.03 0.03 0.06 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-mesh!)
              (clear-rig!)
              {:t 0.0})
      :update (fn [{:keys [time]} scene-state]
                (let [t (+ (:t scene-state) (:dt time))
                      root-rot [0.0 (* 25.0 (Math/sin t)) 0.0]
                      upper-rot [(* 35.0 (Math/sin (* 1.2 t))) 0.0 0.0]
                      lower-rot [(* -50.0 (Math/sin (* 1.6 t))) 0.0 0.0]
                      root (kmath/compose-transform [0.0 -0.1 0.0] root-rot [0.32 0.9 0.32])
                      upper (kmath/chain-transform root [0.0 0.9 0.0] upper-rot [0.30 0.85 0.30])
                      lower (kmath/chain-transform upper [0.0 0.85 0.0] lower-rot [0.28 0.8 0.28])]
                  (set-rig-segments!
                   [{:model root :color [0.9 0.4 0.2]}
                    {:model upper :color [0.2 0.8 0.4]}
                    {:model lower :color [0.2 0.6 0.9]}])
                  (assoc scene-state :t t)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :sweep
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-rig!)
              (let [{:keys [vertices indices]} (kgeom/sweep-mesh)]
                (set-mesh! vertices indices))
              {:t 0.0})
      :update (fn [{:keys [time]} scene-state]
                (let [t (+ (:t scene-state) (:dt time))
                      twist (* 90.0 (Math/sin (* 0.6 t)))
                      {:keys [vertices indices]}
                      (kgeom/sweep-mesh :segments 90
                                        :ring 18
                                        :length 3.2
                                        :radius 0.22
                                        :amp 0.4
                                        :freq 1.3
                                        :twist twist)]
                  (set-mesh! vertices indices)
                  (assoc scene-state :t t)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :heightfield
     {:init (fn [_]
              (set-clear-color! [0.02 0.03 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-rig!)
              (clear-sprites!)
              (set-mesh-style! {:pos [0.0 -0.4 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.55 0.75 0.6]})
              (let [{:keys [vertices indices]}
                    (kgeom/heightfield-mesh 4.0 4.0 80 80 :amp 0.45 :noise-scale 0.8)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :heightfield-particles
     {:init (fn [_]
              (set-clear-color! [0.02 0.03 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-line-segments!)
              (clear-rig!)
              (set-mesh-style! {:pos [0.0 -0.4 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.55 0.75 0.6]})
              (let [height-fn (kgeom/heightfield-height-fn :amp 0.45 :noise-scale 0.8)
                    bounds-lo [-2.0 0.0 -2.0]
                    bounds-hi [2.0 0.0 2.0]
                    {:keys [vertices indices]}
                    (kgeom/heightfield-mesh 4.0 4.0 80 80 :height-fn height-fn)
                    spawn (fn [& _]
                            (let [x (kmath/rand-range (first bounds-lo) (first bounds-hi))
                                  z (kmath/rand-range (nth bounds-lo 2) (nth bounds-hi 2))
                                  y (height-fn x z)
                                  size (kmath/rand-range 8.0 14.0)
                                  life (kmath/rand-range 2.5 5.0)
                                  color [(float (kmath/clamp01 (+ 0.2 (* 0.6 (rand)))))
                                         (float (kmath/clamp01 (+ 0.5 (* 0.4 (rand)))))
                                         (float (kmath/clamp01 (+ 0.2 (* 0.5 (rand)))))
                                         0.85]]
                              (kpart/make-particle {:pos [x (+ y 0.03) z]
                                                    :vel [0.0 0.0 0.0]
                                                    :life life
                                                    :color color
                                                    :size size})))
                    particles (vec (repeatedly 220 spawn))]
                (set-mesh! vertices indices)
                (set-sprite-style! {:size 12.0 :alpha 0.9 :softness 0.6})
                {:height-fn height-fn
                 :bounds-lo bounds-lo
                 :bounds-hi bounds-hi
                 :respawn spawn
                 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      particles (kpart/update-particles-on-heightfield
                                 (:particles scene-state)
                                 dt
                                 {:height-fn (:height-fn scene-state)
                                  :bounds-lo (:bounds-lo scene-state)
                                  :bounds-hi (:bounds-hi scene-state)
                                  :speed 0.7
                                  :noise-scale 0.7
                                  :trail-length 16
                                  :respawn-fn (:respawn scene-state)})
                      {:keys [points colors sizes]} (kpart/particles->sprites particles)
                      line-data (kpart/particles->trails particles)]
                  (set-sprites! points :colors colors :sizes sizes)
                  (set-line-segments! line-data)
                  (assoc scene-state :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :heightfield-sweep
     {:init (fn [_]
              (set-clear-color! [0.02 0.03 0.05 1.0])
              (set-cubes! [])
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (let [height-fn (kgeom/heightfield-height-fn :amp 0.45 :noise-scale 0.8)
                    bounds-lo [-2.0 0.0 -2.0]
                    bounds-hi [2.0 0.0 2.0]
                    points (kgeom/heightfield-points 4.0 4.0 32 32 :height-fn height-fn)
                    colors (mapv (fn [[_ y _]] (kmath/color3 (+ 0.2 (* 0.3 (rand))) (+ 0.5 (* 0.4 (max 0.0 y))) 0.6))
                                 points)
                    spawn (fn [& _]
                            (let [x (kmath/rand-range (first bounds-lo) (first bounds-hi))
                                  z (kmath/rand-range (nth bounds-lo 2) (nth bounds-hi 2))
                                  y (height-fn x z)
                                  size (kmath/rand-range 8.0 12.0)
                                  life (kmath/rand-range 2.5 5.0)]
                              (kpart/make-particle {:pos [x (+ y 0.02) z]
                                                    :vel [0.0 0.0 0.0]
                                                    :life life
                                                    :color [0.6 0.9 0.8 0.9]
                                                    :size size})))
                    particles (vec (repeatedly 180 spawn))]
                (set-point-cloud! points :colors colors)
                (set-mesh-style! {:pos [0.0 0.0 0.0]
                                  :rot [0.0 0.0 0.0]
                                  :scale [1.0 1.0 1.0]
                                  :color [0.7 0.85 0.8]})
                {:height-fn height-fn
                 :bounds-lo bounds-lo
                 :bounds-hi bounds-hi
                 :respawn spawn
                 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      particles (kpart/update-particles-on-heightfield
                                 (:particles scene-state)
                                 dt
                                 {:height-fn (:height-fn scene-state)
                                  :bounds-lo (:bounds-lo scene-state)
                                  :bounds-hi (:bounds-hi scene-state)
                                  :speed 0.75
                                  :noise-scale 0.65
                                  :trail-length 24
                                  :respawn-fn (:respawn scene-state)})
                      paths (->> particles
                                 (map :trail)
                                 (filter #(>= (count %) 6))
                                 (take 8))
                      {:keys [vertices indices]} (kgeom/tube-meshes paths :ring 10 :radius 0.05)]
                  (set-mesh! vertices indices)
                  (assoc scene-state :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :heightfield-animate
     {:init (fn [_]
              (set-clear-color! [0.02 0.03 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (set-mesh-style! {:pos [0.0 -0.4 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.55 0.75 0.6]})
              (let [base-fn (kgeom/heightfield-height-fn :amp 0.25 :noise-scale 0.7)
                    height-fn (fn [x z]
                                (+ (base-fn x z)
                                   (* 0.18 (Math/sin (+ (* 0.9 x) (* 0.7 z))))))
                    {:keys [vertices indices]}
                    (kgeom/heightfield-mesh 4.0 4.0 80 80 :height-fn height-fn)]
                (set-mesh! vertices indices)
                {:t 0.0 :acc 0.0 :base-fn base-fn}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      acc (+ (:acc scene-state) dt)]
                  (if (>= acc 0.2)
                    (let [base-fn (:base-fn scene-state)
                          height-fn (fn [x z]
                                      (+ (base-fn x z)
                                         (* 0.18 (Math/sin (+ (* 0.9 x) (* 0.7 z) (* 1.2 t))))))
                          {:keys [vertices indices]}
                          (kgeom/heightfield-mesh 4.0 4.0 80 80 :height-fn height-fn)]
                      (set-mesh! vertices indices)
                      (assoc scene-state :t t :acc 0.0))
                    (assoc scene-state :t t :acc acc))))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :resource-growth
     {:init (fn [_]
              (set-clear-color! [0.02 0.03 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (let [width 4.0
                    depth 4.0
                    height-fn (kgeom/heightfield-height-fn :amp 0.35 :noise-scale 0.85)
                    {:keys [vertices indices]}
                    (kgeom/heightfield-mesh width depth 64 64 :height-fn height-fn)
                    curve-xy (kgeom/sine-curve-points 360 1.0 3.2 1.1 120)
                    curve-2d (mapv (fn [[x y _]] [x y]) curve-xy)
                    curve-3d (mapv (fn [[x z]]
                                     [x (+ (height-fn x z) 0.02) z])
                                   curve-2d)
                    line-data (polyline->line-data curve-3d [0.2 0.7 1.0])
                    points (kgeom/heightfield-points width depth 14 14 :height-fn height-fn)
                    resource (fn [x z]
                               (let [d (dist-to-polyline [x z] curve-2d)]
                                 (kmath/clamp01 (- 1.0 (/ d 1.6)))))
                    plants (mapv (fn [[x y z]]
                                   (let [r (resource x z)]
                                     {:pos [x y z]
                                      :growth r}))
                                 points)]
                (set-mesh-style! {:pos [0.0 -0.35 0.0]
                                  :rot [0.0 0.0 0.0]
                                  :scale [1.0 1.0 1.0]
                                  :color [0.5 0.75 0.55]})
                (set-mesh! vertices indices)
                (set-line-segments! line-data)
                {:height-fn height-fn
                 :curve-2d curve-2d
                 :plants plants}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      curve-2d (:curve-2d scene-state)
                      resource (fn [x z]
                                 (let [d (dist-to-polyline [x z] curve-2d)]
                                   (kmath/clamp01 (- 1.0 (/ d 1.6)))))
                      plants (mapv (fn [{:keys [pos growth] :as p}]
                                     (let [[x _ z] pos
                                           target (resource x z)
                                           g (+ growth (* (- target growth) (min 1.0 (* dt 0.8))))]
                                       (assoc p :growth g)))
                                   (:plants scene-state))]
                  (set-cubes!
                   (mapv (fn [{:keys [pos growth]}]
                           (let [s (+ 0.08 (* 0.25 growth))]
                             {:id (next-cube-id!)
                              :pos pos
                              :rot [0.0 0.0 0.0]
                              :scale [s (+ s (* 1.8 growth)) s]
                              :color [0.2 (+ 0.4 (* 0.5 growth)) 0.2]}))
                         plants))
                  (assoc scene-state :plants plants)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :surface-particles
     {:init (fn [_]
              (set-clear-color! [0.01 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-line-segments!)
              (clear-rig!)
              (let [radius 1.6
                    height-fn (fn [x z]
                                (let [r2 (- (* radius radius) (+ (* x x) (* z z)))]
                                  (if (pos? r2) (Math/sqrt r2) 0.0)))
                    bounds-lo [(- radius) 0.0 (- radius)]
                    bounds-hi [radius 0.0 radius]
                    {:keys [vertices indices]} (kgeom/uv-sphere 28 40)
                    spawn (fn [& _]
                            (let [x (kmath/rand-range (first bounds-lo) (first bounds-hi))
                                  z (kmath/rand-range (nth bounds-lo 2) (nth bounds-hi 2))
                                  y (height-fn x z)
                                  size (kmath/rand-range 8.0 13.0)
                                  life (kmath/rand-range 2.5 4.5)
                                  color [0.5 0.9 0.95 0.85]]
                              (kpart/make-particle {:pos [x (+ y 0.02) z]
                                                    :vel [0.0 0.0 0.0]
                                                    :life life
                                                    :color color
                                                    :size size})))
                    particles (vec (repeatedly 180 spawn))]
                (set-mesh-style! {:pos [0.0 0.0 0.0]
                                  :rot [0.0 0.0 0.0]
                                  :scale [1.0 1.0 1.0]
                                  :color [0.3 0.4 0.55]})
                (set-mesh! vertices indices)
                (set-sprite-style! {:size 10.0 :alpha 0.9 :softness 0.7})
                {:height-fn height-fn
                 :bounds-lo bounds-lo
                 :bounds-hi bounds-hi
                 :respawn spawn
                 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      particles (kpart/update-particles-on-heightfield
                                 (:particles scene-state)
                                 dt
                                 {:height-fn (:height-fn scene-state)
                                  :bounds-lo (:bounds-lo scene-state)
                                  :bounds-hi (:bounds-hi scene-state)
                                  :speed 0.6
                                  :noise-scale 1.1
                                  :wrap? false
                                  :height-offset 0.02
                                  :trail-length 14
                                  :respawn-fn (:respawn scene-state)})
                      {:keys [points colors sizes]} (kpart/particles->sprites particles)
                      line-data (kpart/particles->trails particles)]
                  (set-sprites! points :colors colors :sizes sizes)
                  (set-line-segments! line-data)
                  (assoc scene-state :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :polyhedron-particles
     {:init (fn [_]
              (set-clear-color! [0.01 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-line-segments!)
              (clear-rig!)
              (let [poly (kgeom/icosahedron-polyhedron 2.2)
                    sources (kgeom/polyhedron-face-centers poly)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh poly)
                    spawn (fn [& _]
                            (first (kpart/emit-from-points
                                    [(rand-nth sources)]
                                    :direction-fn (fn [p] p)
                                    :speed-range [0.5 1.1]
                                    :life-range [2.5 4.5]
                                    :size-range [9.0 16.0]
                                    :jitter 0.25)))
                    particles (vec (repeatedly (count sources) spawn))]
                (set-mesh-style! {:pos [0.0 0.0 0.0]
                                  :rot [0.0 0.0 0.0]
                                  :scale [1.0 1.0 1.0]
                                  :color [0.25 0.4 0.65]})
                (set-mesh! vertices indices)
                (set-sprite-style! {:size 12.0 :alpha 0.85 :softness 0.6})
                {:sources sources
                 :respawn spawn
                 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      fields [(kpart/make-noise-field 0.8 0.4)
                              (kpart/make-vortex-field [0.0 0.0 0.0] 0.7)]
                      particles (kpart/update-particles-advanced
                                 (:particles scene-state)
                                 dt
                                 {:fields fields
                                  :ground-y -2.2
                                  :trail-length 18
                                  :respawn-fn (:respawn scene-state)})
                      {:keys [points colors sizes]} (kpart/particles->sprites particles)
                      line-data (kpart/particles->trails particles)]
                  (set-sprites! points :colors colors :sizes sizes)
                  (set-line-segments! line-data)
                  (assoc scene-state :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :polyhedron-vertices
     {:init (fn [_]
              (set-clear-color! [0.01 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-line-segments!)
              (clear-rig!)
              (let [poly (kgeom/dodecahedron-polyhedron 2.4)
                    sources (kgeom/polyhedron-vertices poly)
                    colors (mapv (fn [[x y z]]
                                   [(float (kmath/clamp01 (+ 0.5 (* 0.4 x))))
                                    (float (kmath/clamp01 (+ 0.5 (* 0.4 y))))
                                    (float (kmath/clamp01 (+ 0.5 (* 0.4 z))))
                                    0.85])
                                 sources)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh poly)
                    source-data (mapv (fn [p c] {:pos p :color c}) sources colors)
                    spawn (fn [& _]
                            (let [{:keys [pos color]} (rand-nth source-data)]
                              (kpart/make-particle {:pos pos
                                                    :vel [0.0 0.0 0.0]
                                                    :life (kmath/rand-range 2.0 4.0)
                                                    :color color
                                                    :size (kmath/rand-range 9.0 15.0)})))
                    particles (vec (repeatedly 140 spawn))]
                (set-mesh-style! {:pos [0.0 0.0 0.0]
                                  :rot [0.0 0.0 0.0]
                                  :scale [1.0 1.0 1.0]
                                  :color [0.25 0.35 0.55]})
                (set-mesh! vertices indices)
                (set-sprite-style! {:size 12.0 :alpha 0.85 :softness 0.6})
                {:respawn spawn
                 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      fields [(kpart/make-noise-field 0.8 0.35)]
                      particles (kpart/update-particles-advanced
                                 (:particles scene-state)
                                 dt
                                 {:fields fields
                                  :ground-y -2.2
                                  :trail-length 18
                                  :respawn-fn (:respawn scene-state)})
                      {:keys [points colors sizes]} (kpart/particles->sprites particles)
                      line-data (kpart/particles->trails particles)]
                  (set-sprites! points :colors colors :sizes sizes)
                  (set-line-segments! line-data)
                  (assoc scene-state :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :polyhedron-face-centers
     {:init (fn [_]
              (set-clear-color! [0.01 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-line-segments!)
              (clear-rig!)
              (let [poly (kgeom/icosahedron-polyhedron 2.2)
                    sources (kgeom/polyhedron-face-centers poly)
                    colors (mapv (fn [[x y z]]
                                   [(float (kmath/clamp01 (+ 0.5 (* 0.5 x))))
                                    (float (kmath/clamp01 (+ 0.5 (* 0.5 y))))
                                    (float (kmath/clamp01 (+ 0.5 (* 0.5 z))))
                                    0.85])
                                 sources)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh poly)
                    source-data (mapv (fn [p c] {:pos p :color c}) sources colors)
                    spawn (fn [& _]
                            (let [{:keys [pos color]} (rand-nth source-data)]
                              (kpart/make-particle {:pos pos
                                                    :vel [0.0 0.0 0.0]
                                                    :life (kmath/rand-range 2.0 4.0)
                                                    :color color
                                                    :size (kmath/rand-range 9.0 15.0)})))
                    particles (vec (repeatedly 200 spawn))]
                (set-mesh-style! {:pos [0.0 0.0 0.0]
                                  :rot [0.0 0.0 0.0]
                                  :scale [1.0 1.0 1.0]
                                  :color [0.2 0.35 0.6]})
                (set-mesh! vertices indices)
                (set-sprite-style! {:size 12.0 :alpha 0.85 :softness 0.6})
                {:respawn spawn
                 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      fields [(kpart/make-vortex-field [0.0 0.0 0.0] 0.6)]
                      particles (kpart/update-particles-advanced
                                 (:particles scene-state)
                                 dt
                                 {:fields fields
                                  :ground-y -2.2
                                  :trail-length 18
                                  :respawn-fn (:respawn scene-state)})
                      {:keys [points colors sizes]} (kpart/particles->sprites particles)
                      line-data (kpart/particles->trails particles)]
                  (set-sprites! points :colors colors :sizes sizes)
                  (set-line-segments! line-data)
                  (assoc scene-state :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :polyhedron-refine
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (set-wireframe-overlay! {:enabled? true
                                       :line-width 1.0
                                       :color [0.1 0.1 0.15]})
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.7 0.75 0.85]})
              (let [poly (kgeom/cube-polyhedron 2.0)
                    refined (kgeom/refine-polyhedron poly 2)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh refined)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (set-wireframe-overlay! {:enabled? false}))}

     :polyhedron-fractal
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (set-wireframe-overlay! {:enabled? false})
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.75 0.7 0.55]})
              (let [poly (kgeom/cube-polyhedron 2.0)
                    fractal (kgeom/fractalize-polyhedron poly 3 0.35)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh fractal)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :cut-cube
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (set-wireframe-overlay! {:enabled? true
                                       :line-width 1.0
                                       :color [0.12 0.12 0.15]})
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.65 0.78 0.9]})
              (let [poly (kgeom/cut-cube-polyhedron 2.0)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh poly)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (set-wireframe-overlay! {:enabled? false}))}

     :box
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-line-segments!)
              (clear-rig!)
              (set-wireframe-overlay! {:enabled? true
                                       :line-width 1.0
                                       :color [0.12 0.12 0.15]})
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.85 0.72 0.55]})
              (let [poly (kgeom/box-polyhedron 2.4 1.4 1.0)
                    {:keys [vertices indices]} (kgeom/polyhedron-mesh poly)]
                (set-mesh! vertices indices))
              nil)
      :update (fn [_ scene-state] scene-state)
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (set-wireframe-overlay! {:enabled? false}))}

     :isosurface-points
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-rig!)
              (clear-sprites!)
              (clear-line-segments!)
              (set-wireframe-overlay! {:enabled? false})
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.7 0.8 0.95]})
              (let [points (kgeom/random-points 16 [-1.0 -0.6 -1.0] [1.0 0.6 1.0])
                    colors (mapv (fn [[x y z]]
                                   [(float (kmath/clamp01 (+ 0.5 (* 0.4 x))))
                                    (float (kmath/clamp01 (+ 0.5 (* 0.4 y))))
                                    (float (kmath/clamp01 (+ 0.5 (* 0.4 z))))])
                                 points)
                    {:keys [vertices indices]}
                    (ksdf/isosurface-from-points points {:radius 0.35
                                                         :threshold 1.0
                                                         :min [-1.4 -1.0 -1.4]
                                                         :max [1.4 1.0 1.4]
                                                         :res 32})]
                (set-point-cloud! points :colors colors)
                (set-mesh! vertices indices)
                {:t 0.0 :acc 0.0 :points points}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      acc (+ (:acc scene-state) dt)]
                  (if (>= acc 0.3)
                    (let [threshold (+ 0.9 (* 0.4 (Math/sin (* 0.7 t))))
                          {:keys [vertices indices]}
                          (ksdf/isosurface-from-points (:points scene-state)
                                                       {:radius 0.35
                                                        :threshold threshold
                                                        :min [-1.4 -1.0 -1.4]
                                                        :max [1.4 1.0 1.4]
                                                        :res 32})]
                      (set-mesh! vertices indices)
                      (assoc scene-state :t t :acc 0.0))
                    (assoc scene-state :t t :acc acc))))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :isosurface-curves
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-rig!)
              (clear-sprites!)
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.7 0.7 0.95]})
              (let [curve (kgeom/sine-curve-points 360 1.0 2.2 1.0 120)
                    {:keys [vertices indices]}
                    (ksdf/isosurface-from-curves [curve]
                                                 {:radius 0.25
                                                  :threshold 1.0
                                                  :min [-2.0 -1.2 -1.2]
                                                  :max [2.0 1.2 1.2]
                                                  :res 32})]
                (set-mesh! vertices indices)
                {:t 0.0 :acc 0.0 :curve curve}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      acc (+ (:acc scene-state) dt)]
                  (if (>= acc 0.25)
                    (let [threshold (+ 0.8 (* 0.4 (Math/sin (* 0.8 t))))
                          {:keys [vertices indices]}
                          (ksdf/isosurface-from-curves [(:curve scene-state)]
                                                       {:radius 0.25
                                                        :threshold threshold
                                                        :min [-2.0 -1.2 -1.2]
                                                        :max [2.0 1.2 1.2]
                                                        :res 32})]
                      (set-mesh! vertices indices)
                      (assoc scene-state :t t :acc 0.0))
                    (assoc scene-state :t t :acc acc))))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :isosurface-particles
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-rig!)
              (clear-sprites!)
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.7 0.7 0.95]})
              (let [sources (kgeom/circle-points 1.6 48)
                    spawn (fn [& _]
                            (first (kpart/emit-from-points
                                    [(rand-nth sources)]
                                    :tangent? true
                                    :speed-range [0.4 0.9]
                                    :life-range [2.0 4.0]
                                    :size-range [8.0 12.0]
                                    :jitter 0.2)))
                    particles (vec (repeatedly 140 spawn))]
                {:t 0.0 :acc 0.0 :particles particles :respawn spawn}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      acc (+ (:acc scene-state) dt)
                      fields [(kpart/make-noise-field 0.7 0.5)]
                      particles (kpart/update-particles-advanced
                                 (:particles scene-state)
                                 dt
                                 {:fields fields
                                  :ground-y -1.2
                                  :trail-length 20
                                  :respawn-fn (:respawn scene-state)})]
                  (if (>= acc 0.3)
                    (let [curves (->> particles (map :trail) (filter #(>= (count %) 6)) (take 12))
                          {:keys [vertices indices]}
                          (ksdf/isosurface-from-curves curves
                                                       {:radius 0.22
                                                        :threshold 0.9
                                                        :min [-2.0 -1.2 -2.0]
                                                        :max [2.0 1.2 2.0]
                                                        :res 30})]
                      (set-mesh! vertices indices)
                      (assoc scene-state :t t :acc 0.0 :particles particles))
                    (assoc scene-state :t t :acc acc :particles particles))))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :particles
     {:init (fn [_]
              (set-clear-color! [0.01 0.01 0.04 1.0])
              (set-cubes! [])
              (clear-rig!)
              (set-mesh-index-count! 0)
              (let [particles (kpart/init-particles 400 1.6)
                    {:keys [points colors]} (kpart/particles->points particles)]
                (upload-point-cloud! points colors)
                {:t 0.0 :particles particles}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      particles (kpart/update-particles (:particles scene-state) dt t)
                      {:keys [points colors]} (kpart/particles->points particles)]
                  (upload-point-cloud! points colors)
                  (assoc scene-state :t t :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :particle-fields
     {:init (fn [_]
              (set-clear-color! [0.01 0.01 0.04 1.0])
              (set-cubes! [])
              (clear-rig!)
              (clear-point-cloud!)
              (clear-sprites!)
              (clear-mesh!)
              (clear-line-segments!)
              (let [sources (kgeom/circle-points 2.2 64)
                    spawn (fn [_]
                            (first (kpart/emit-from-points
                                    [(rand-nth sources)]
                                    :tangent? true
                                    :speed-range [0.4 1.0]
                                    :life-range [2.0 4.0]
                                    :size-range [10.0 18.0]
                                    :jitter 0.2)))
                    particles (mapv spawn sources)]
                {:t 0.0
                 :sources sources
                 :particles particles
                 :respawn spawn}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      fields [(kpart/make-gravity-field 0.15)
                              (kpart/make-attractor-field [0.0 0.0 0.0] 1.1)
                              (kpart/make-vortex-field [0.0 0.0 0.0] 0.9)
                              (kpart/make-noise-field 0.6 0.5)]
                      particles (kpart/update-particles-advanced
                                 (:particles scene-state)
                                 dt
                                 {:fields fields
                                  :ground-y -1.2
                                  :trail-length 20
                                  :respawn-fn (:respawn scene-state)})
                      {:keys [points colors sizes]} (kpart/particles->sprites particles)
                      line-data (kpart/particles->trails particles)]
                  (set-sprites! points :colors colors :sizes sizes)
                  (set-line-segments! line-data)
                  (assoc scene-state :t t :particles particles)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] (clear-line-segments!))}

     :sdf
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-rig!)
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.7 0.7 0.95]})
              (let [{:keys [vertices indices]}
                    (ksdf/marching-tetrahedra ksdf/sdf-metaballs {:min [-1.4 -1.1 -1.1]
                                                                  :max [1.4 1.1 1.1]
                                                                  :res 36
                                                                  :t 0.0})]
                (set-mesh! vertices indices))
              {:t 0.0 :acc 0.0})
      :update (fn [{:keys [time]} scene-state]
                (let [scene-state (or scene-state {:t 0.0 :acc 0.0})
                      dt (:dt time)
                      t (+ (:t scene-state) dt)
                      acc (+ (:acc scene-state) dt)]
                  (if (>= acc 0.2)
                    (let [{:keys [vertices indices]}
                          (ksdf/marching-tetrahedra ksdf/sdf-metaballs {:min [-1.4 -1.1 -1.1]
                                                                        :max [1.4 1.1 1.1]
                                                                        :res 36
                                                                        :t t})]
                      (set-mesh! vertices indices)
                      (assoc scene-state :t t :acc 0.0))
                    (assoc scene-state :t t :acc acc))))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :spring
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-rig!)
              (set-mesh-index-count! 0)
              (let [{:keys [nodes springs]} (kspring/make-spring-grid 10 10 0.22)
                    points (mapv :pos nodes)
                    colors (mapv (fn [{:keys [fixed?]}]
                                   (if fixed? [1.0 0.9 0.2] [0.8 0.8 0.9]))
                                 nodes)
                    lines (kspring/spring-lines nodes springs)]
                (upload-point-cloud! points colors)
                (upload-spring-lines! lines)
                {:nodes nodes :springs springs}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      nodes (kspring/update-spring-grid (:nodes scene-state) (:springs scene-state) dt)
                      points (mapv :pos nodes)
                      colors (mapv (fn [{:keys [fixed?]}]
                                     (if fixed? [1.0 0.9 0.2] [0.8 0.8 0.9]))
                                   nodes)
                      lines (kspring/spring-lines nodes (:springs scene-state))]
                  (upload-point-cloud! points colors)
                  (upload-spring-lines! lines)
                  (assoc scene-state :nodes nodes)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :spring-isosurface
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (set-cubes! [])
              (clear-rig!)
              (clear-sprites!)
              (clear-line-segments!)
              (set-mesh-style! {:pos [0.0 0.0 0.0]
                                :rot [0.0 0.0 0.0]
                                :scale [1.0 1.0 1.0]
                                :color [0.7 0.75 0.9]})
              (let [{:keys [nodes springs]} (kspring/make-spring-grid 8 8 0.35)
                    points (mapv :pos nodes)
                    colors (mapv (fn [{:keys [fixed?]}]
                                   (if fixed? [1.0 0.85 0.2] [0.7 0.75 0.9]))
                                 nodes)
                    lines (kspring/spring-lines nodes springs)
                    curves (->> springs
                                (map (fn [[i j _]]
                                       [(get-in nodes [i :pos])
                                        (get-in nodes [j :pos])]))
                                (take 80)
                                (vec))
                    {:keys [vertices indices]}
                    (ksdf/isosurface-from-curves curves
                                                 {:radius 0.18
                                                  :threshold 0.9
                                                  :min [-1.6 -0.6 -1.6]
                                                  :max [1.6 1.2 1.6]
                                                  :res 26})]
                (set-point-cloud! points :colors colors)
                (set-line-segments! lines)
                (set-mesh! vertices indices)
                {:nodes nodes :springs springs :acc 0.0}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      acc (+ (:acc scene-state) dt)
                      nodes (kspring/update-spring-grid (:nodes scene-state) (:springs scene-state) dt)
                      points (mapv :pos nodes)
                      colors (mapv (fn [{:keys [fixed?]}]
                                     (if fixed? [1.0 0.85 0.2] [0.7 0.75 0.9]))
                                   nodes)
                      lines (kspring/spring-lines nodes (:springs scene-state))]
                  (set-point-cloud! points :colors colors)
                  (set-line-segments! lines)
                  (if (>= acc 0.35)
                    (let [curves (->> (:springs scene-state)
                                      (map (fn [[i j _]]
                                             [(get-in nodes [i :pos])
                                              (get-in nodes [j :pos])]))
                                      (take 80)
                                      (vec))
                          {:keys [vertices indices]}
                          (ksdf/isosurface-from-curves curves
                                                       {:radius 0.18
                                                        :threshold 0.9
                                                        :min [-1.6 -0.6 -1.6]
                                                        :max [1.6 1.2 1.6]
                                                        :res 26})]
                      (set-mesh! vertices indices)
                      (assoc scene-state :nodes nodes :acc 0.0))
                    (assoc scene-state :nodes nodes :acc acc))))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _]
                 (clear-point-cloud!)
                 (clear-line-segments!))}

     :ecosystem
     {:init (fn [_]
              (set-clear-color! [0.02 0.02 0.05 1.0])
              (clear-rig!)
              (clear-point-cloud!)
              (clear-mesh!)
              (clear-spring-lines!)
              (let [plants (repeatedly 30 (fn []
                                            {:pos [(kmath/rand-range -1.2 1.2) 0.0 (kmath/rand-range -1.0 1.0)]
                                             :energy 1.0}))
                    herbivores (repeatedly 14 (fn []
                                                {:pos [(kmath/rand-range -1.4 1.4) 0.0 (kmath/rand-range -1.2 1.2)]
                                                 :vel [0.0 0.0 0.0]
                                                 :energy 1.2}))
                    predators (repeatedly 6 (fn []
                                              {:pos [(kmath/rand-range -1.4 1.4) 0.0 (kmath/rand-range -1.2 1.2)]
                                               :vel [0.0 0.0 0.0]
                                               :energy 1.6}))]
                (set-cubes! [])
                {:plants (vec plants)
                 :herbivores (vec herbivores)
                 :predators (vec predators)
                 :t 0.0}))
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      plants (:plants scene-state)
                      herbivores
                      (mapv
                       (fn [{:keys [pos energy] :as h}]
                         (let [food (when (seq plants)
                                      (apply min-key #(dist2 pos (:pos %)) plants))
                               jitter [(kmath/rand-range -0.35 0.35) 0.0 (kmath/rand-range -0.35 0.35)]
                               vel (if food
                                     (let [d (dist2 pos (:pos food))]
                                       (if (< d 0.04)
                                         jitter
                                         (steer pos (:pos food) 1.2)))
                                     jitter)
                               [vx vy vz] vel
                               [px py pz] pos
                               npos [(+ px (* vx dt)) 0.0 (+ pz (* vz dt))]
                               energy (- energy (* 0.08 dt))
                               eaten? (and food (< (dist2 npos (:pos food)) 0.03))]
                           (cond-> (assoc h :pos npos :vel vel :energy energy)
                             eaten? (update :energy + 0.6))))
                       (:herbivores scene-state))
                      eaten-plants
                      (set (keep (fn [{:keys [pos energy]}]
                                   (when (> energy 1.5) pos))
                                 herbivores))
                      plants
                      (vec (remove (fn [{:keys [pos]}]
                                     (some #(<= (dist2 pos %) 0.03) eaten-plants))
                                   plants))
                      herbivores
                      (vec (remove #(<= (:energy %) 0.0) herbivores))
                      born-herbivores
                      (mapcat (fn [{:keys [pos energy]}]
                                (when (> energy 2.2)
                                  [{:pos [(+ (first pos) (kmath/rand-range -0.1 0.1))
                                          0.0
                                          (+ (nth pos 2) (kmath/rand-range -0.1 0.1))]
                                    :vel [0.0 0.0 0.0]
                                    :energy 1.0}]))
                              herbivores)
                      herbivores
                      (vec (concat (mapv (fn [h]
                                           (if (> (:energy h) 2.2)
                                             (assoc h :energy 1.2)
                                             h))
                                         herbivores)
                                   born-herbivores))
                      predators
                      (mapv
                       (fn [{:keys [pos energy] :as p}]
                         (let [target (when (seq herbivores)
                                        (apply min-key #(dist2 pos (:pos %)) herbivores))
                               jitter [(kmath/rand-range -0.25 0.25) 0.0 (kmath/rand-range -0.25 0.25)]
                               vel (if target
                                     (let [d (dist2 pos (:pos target))]
                                       (if (< d 0.06)
                                         jitter
                                         (steer pos (:pos target) 1.6)))
                                     jitter)
                               [vx vy vz] vel
                               [px py pz] pos
                               npos [(+ px (* vx dt)) 0.0 (+ pz (* vz dt))]
                               energy (- energy (* 0.12 dt))
                               eaten? (and target (< (dist2 npos (:pos target)) 0.04))]
                           (cond-> (assoc p :pos npos :vel vel :energy energy)
                             eaten? (update :energy + 0.8))))
                       (:predators scene-state))
                      eaten-herbivores
                      (set (keep (fn [{:keys [pos energy]}]
                                   (when (> energy 2.0) pos))
                                 predators))
                      herbivores
                      (vec (remove (fn [{:keys [pos]}]
                                     (some #(<= (dist2 pos %) 0.04) eaten-herbivores))
                                   herbivores))
                      predators
                      (vec (remove #(<= (:energy %) 0.0) predators))
                      born-predators
                      (mapcat (fn [{:keys [pos energy]}]
                                (when (> energy 2.6)
                                  [{:pos [(+ (first pos) (kmath/rand-range -0.1 0.1))
                                          0.0
                                          (+ (nth pos 2) (kmath/rand-range -0.1 0.1))]
                                    :vel [0.0 0.0 0.0]
                                    :energy 1.4}]))
                              predators)
                      predators
                      (vec (concat (mapv (fn [p]
                                           (if (> (:energy p) 2.6)
                                             (assoc p :energy 1.6)
                                             p))
                                         predators)
                                   born-predators))
                      plants
                      (vec (concat plants
                                   (repeatedly (max 0 (- 30 (count plants)))
                                               (fn []
                                                 {:pos [(kmath/rand-range -1.2 1.2) 0.0 (kmath/rand-range -1.0 1.0)]
                                                  :energy 1.0}))))]
                  (set-cubes!
                   (vec
                    (concat
                     (map (fn [{:keys [pos]}]
                            {:id (next-cube-id!)
                             :pos pos :rot [0.0 0.0 0.0] :scale [0.12 0.12 0.12]
                             :color [0.2 0.9 0.3]})
                          plants)
                     (map (fn [{:keys [pos]}]
                            {:id (next-cube-id!)
                             :pos pos :rot [0.0 0.0 0.0] :scale [0.16 0.16 0.16]
                             :color [0.9 0.9 0.2]})
                          herbivores)
                     (map (fn [{:keys [pos]}]
                            {:id (next-cube-id!)
                             :pos pos :rot [0.0 0.0 0.0] :scale [0.2 0.2 0.2]
                             :color [0.9 0.2 0.2]})
                          predators))))
                  (assoc scene-state
                         :plants plants
                         :herbivores herbivores
                         :predators predators
                         :t t)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _] nil)}

     :sdf-demo
     {:init (fn [_]
              (set-clear-color! [0.01 0.01 0.04 1.0])
              (set-cubes! [])
              (clear-point-cloud!)
              (clear-line-segments!)
              (clear-sprites!)
              (clear-mesh!)
              {:t 0.0 :res 40})
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      sphere (ksdf/sphere-sdf [0.0 (* 0.2 (Math/sin (* 0.8 t))) 0.0]
                                              (+ 0.6 (* 0.2 (Math/abs (Math/sin (* 0.5 t))))))
                      box (ksdf/box-sdf [(Math/sin (* 0.35 t)) 0.0 (Math/cos (* 0.4 t))]
                                        [1.2 1.0 1.2])
                      torus (ksdf/torus-sdf [0.0 0.3 0.0] 0.7 0.18)
                      blend (+ 0.1 (* 0.5 (Math/abs (Math/sin (* 0.4 t)))))
                      union (ksdf/sdf-union sphere torus blend)
                      carved (ksdf/sdf-difference union box (* 0.25 (Math/abs (Math/cos (* 0.2 t)))))
                      {:keys [vertices indices]}
                      (ksdf/marching-tetrahedra carved
                                                {:min [-1.5 -1.1 -1.5]
                                                 :max [1.5 1.2 1.5]
                                                 :res (:res scene-state)
                                                 :t t})
                      circle (kgeom/circle-points 1.9 64)
                      line-data (polyline->line-data circle [0.4 0.8 1.0])
                      colors (mapv (fn [[_ y _]]
                                     (kmath/rainbow-color (* 0.25 (+ y 1.0))))
                                   circle)]
                  (set-mesh! vertices indices)
                  (set-mesh-style! {:pos [0.0 0.0 0.0]
                                    :rot [0.0 0.0 0.0]
                                    :scale [1.0 1.0 1.0]
                                    :color [0.35 0.75 0.95]})
                  (set-line-segments! line-data)
                  (set-point-cloud! circle :colors colors)
                  (assoc scene-state :t t)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _]
                 (clear-line-segments!)
                 (clear-point-cloud!))}

     :sweep-live
     {:init (fn [_]
              (set-clear-color! [0.02 0.03 0.06 1.0])
              (set-cubes! [])
              (clear-sprites!)
              (clear-line-segments!)
              (clear-mesh!)
              (set-mesh-style! {:color [0.65 0.7 0.9]})
              {:t 0.0})
      :update (fn [{:keys [time]} scene-state]
                (let [dt (:dt time)
                      t (+ (:t scene-state) dt)
                      diameter (+ 0.8 (* 0.4 (Math/sin (* 0.6 t))))
                      path (kgeom/spiral-points diameter (+ diameter 1.0) 4.5 4 140)
                      segments (max 24 (int (+ 32 (* 16 (Math/abs (Math/sin (* 0.4 t)))))))
                      twist (* 180 (Math/sin (* 0.45 t)))
                      taper (max 0.5 (+ 0.5 (* 0.3 (Math/sin (* 0.3 t)))))
                      {:keys [vertices indices]} (kgeom/sweep-mesh
                                                  :segments segments
                                                  :ring 18
                                                  :length 4.8
                                                  :radius 0.18
                                                  :amp 0.45
                                                  :freq 1.1
                                                  :twist twist
                                                  :taper taper)
                      line-data (polyline->line-data path [0.9 0.6 0.3])
                      colors (let [n (count path)]
                               (mapv (fn [idx]
                                       (kmath/rainbow-color (/ idx (double (max 1 (dec n))))))
                                     (range n)))]
                  (set-mesh! vertices indices)
                  (set-line-segments! line-data)
                  (set-sprites! path :colors colors :sizes (vec (repeat (count path) 10.0)))
                  (assoc scene-state :t t)))
      :render (fn [ctx _] (default-render ctx))
      :cleanup (fn [_ _]
                 (clear-line-segments!)
                 (clear-sprites!))}}))
