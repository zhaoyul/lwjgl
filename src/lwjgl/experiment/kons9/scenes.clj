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
                upload-point-cloud!
                set-mesh!
                clear-mesh!
                set-mesh-style!
                set-mesh-index-count!
                upload-spring-lines!
                clear-spring-lines!
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
      :cleanup (fn [_ _] nil)}}))
