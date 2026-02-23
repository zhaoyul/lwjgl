(ns lwjgl.experiment.kons9.showcase
  "Kons-9 功能展示 - 全面演示 Clojure 版的所有功能"
  (:require [lwjgl.experiment.hotreload :as hot]
            [lwjgl.experiment.kons9.api :as api]
            [lwjgl.experiment.kons9.geometry :as kgeom]
            [lwjgl.experiment.kons9.math :as kmath]
            [lwjgl.experiment.kons9.geometry.curves :as curves]
            [lwjgl.experiment.kons9.geometry.primitives :as prim]
            [lwjgl.experiment.kons9.geometry.subdiv :as subdiv]
            [lwjgl.experiment.kons9.geometry.sweep :as sweep]
            [lwjgl.experiment.kons9.animation.boid :as boid]
            [lwjgl.experiment.kons9.animation.lsystem :as lsys]
            [lwjgl.experiment.kons9.io.obj :as obj]
            [lwjgl.experiment.kons9.io.stl :as stl]))

;; ============================================
;; 辅助函数
;; ============================================

(defn- mesh->cubes
  "将网格顶点转换为小立方体"
  [vertices size]
  (map-indexed (fn [idx [x y z]]
                 {:id idx
                  :pos [x y z]
                  :rot [0.0 0.0 0.0]
                  :scale [size size size]
                  :color [(+ 0.5 (* 0.5 (Math/sin x)))
                          (+ 0.5 (* 0.5 (Math/cos y)))
                          (+ 0.5 (* 0.5 (Math/sin z)))]})
               vertices))

(defn- paths->line-segments
  "将路径列表转换为线段数据"
  [paths color]
  (mapcat (fn [path]
            (mapcat (fn [[a b]]
                      (let [[r g b] color]
                        [(float (first a)) (float (second a)) (float (nth a 2)) r g b
                         (float (first b)) (float (second b)) (float (nth b 2)) r g b]))
                    (partition 2 1 path)))
          paths))

(defn- upload-faces-mesh!
  "将 {:vertices :faces} 网格转换为可渲染网格并上传。"
  [set-mesh! mesh]
  (let [{:keys [vertices indices]} (kgeom/mesh-from-faces (:vertices mesh) (:faces mesh))]
    (set-mesh! vertices indices)))

;; ============================================
;; 1. 基础几何展示
;; ============================================

(defn basics-scene []
  {:init (fn [api]
           (let [{:keys [set-clear-color! set-cubes! set-point-cloud!
                         clear-point-cloud! default-render]} api]
             (set-clear-color! [0.05 0.05 0.08 1.0])
             ;; 彩色立方体阵列
             (set-cubes! (for [x (range -2 3)
                               y (range -1 2)
                               z (range -1 2)]
                           {:id (+ (* x 100) (* y 10) z)
                            :pos [(* x 0.8) (* y 0.8) (* z 0.8)]
                            :rot [(* x 15.0) (* y 15.0) (* z 15.0)]
                            :scale [0.3 0.3 0.3]
                            :color [(mod (+ x 2) 3)
                                    (mod (+ y 1) 3)
                                    (mod (+ z 1) 3)]}))
             ;; 随机点云
             (let [points (repeatedly 200 (fn []
                                            [(kmath/rand-range -3.0 3.0)
                                             (kmath/rand-range -2.0 2.0)
                                             (kmath/rand-range -2.0 2.0)]))
                   colors (repeatedly 200 #(kmath/color3 (rand) (rand) (rand)))]
               (set-point-cloud! points :colors colors))
             nil))

   :update (fn [api scene-state]
             (let [{:keys [update-cubes! time]} api
                   {:keys [dt]} time]
               (update-cubes!
                (fn [cubes]
                  (mapv (fn [c]
                          (update c :rot (fn [[rx ry rz]]
                                           [(+ rx (* 20.0 dt))
                                            (+ ry (* 30.0 dt))
                                            (+ rz (* 10.0 dt))])))
                        cubes)))
               scene-state))

   :render (fn [api _]
             ((:default-render api)))

   :cleanup (fn [_ _] nil)})

;; ============================================
;; 2. 曲线艺术展示
;; ============================================

(defn curves-scene []
  {:init (fn [api]
           (let [{:keys [set-clear-color! set-cubes! clear-point-cloud!
                         set-line-segments! clear-line-segments! next-cube-id!]} api]
             (set-clear-color! [0.02 0.02 0.04 1.0])
             (clear-point-cloud!)

             ;; Bezier 曲线控制点
             (let [p0 [0.0 0.0 0.0]
                   p1 [1.0 2.0 0.0]
                   p2 [2.0 2.0 0.0]
                   p3 [3.0 0.0 0.0]
                   curve-points (curves/make-bezier-curve p0 p1 p2 p3 50)]
               ;; 控制点立方体
               (set-cubes! [{:id (next-cube-id!) :pos p0 :scale [0.15 0.15 0.15] :color [1.0 0.0 0.0]}
                            {:id (next-cube-id!) :pos p1 :scale [0.15 0.15 0.15] :color [0.0 1.0 0.0]}
                            {:id (next-cube-id!) :pos p2 :scale [0.15 0.15 0.15] :color [0.0 0.0 1.0]}
                            {:id (next-cube-id!) :pos p3 :scale [0.15 0.15 0.15] :color [1.0 1.0 0.0]}])
               ;; 曲线线段
               (set-line-segments!
                (paths->line-segments [curve-points] [0.9 0.4 0.2])))

             ;; 螺旋线
             (let [helix (curves/make-helix-points 1.5 4.0 3 100)]
               (set-line-segments!
                (paths->line-segments [helix] [0.2 0.8 0.4])
                :append? true))

             nil))

   :update (fn [api scene-state]
             scene-state)

   :render (fn [api _] ((:default-render api)))
   :cleanup (fn [api _]
              ((:clear-line-segments! api)))})

;; ============================================
;; 3. 细分网格展示
;; ============================================

(defn subdiv-scene []
  {:init (fn [api]
           (let [{:keys [set-clear-color! set-mesh! clear-mesh!
                         set-cubes! next-cube-id!]} api]
             (set-clear-color! [0.03 0.03 0.05 1.0])
             ;; 原始立方体
             (let [box (prim/make-box-mesh-data 1.0 1.0 1.0)]
               (upload-faces-mesh! set-mesh! box)
               (set-cubes! [{:id (next-cube-id!)
                             :pos [-1.5 0.0 0.0]
                             :scale [0.5 0.5 0.5]
                             :color [0.8 0.2 0.2]
                             :label "Original"}]))
             {:subdiv-level 0 :max-level 3}))

   :update (fn [api scene-state]
             (let [{:keys [set-mesh!]} api
                   {:keys [subdiv-level max-level t]} scene-state
                   t (or t 0)]
               (when (> t 2.0)
                 (let [new-level (mod (inc (or subdiv-level 0)) (inc max-level))
                       box (prim/make-box-mesh-data 1.0 1.0 1.0)
                       result (case new-level
                                0 box
                                1 (subdiv/subdivide-catmull-clark
                                   (:vertices box) (:faces box))
                                2 (subdiv/subdivide-times
                                   (:vertices box) (:faces box)
                                   subdiv/subdivide-catmull-clark 2)
                                3 (subdiv/subdivide-times
                                   (:vertices box) (:faces box)
                                   subdiv/subdivide-catmull-clark 3))]
                   (upload-faces-mesh! set-mesh! result)
                   (assoc scene-state
                          :subdiv-level new-level
                          :t 0
                          :vert-count (count (:vertices result))))
                 (assoc scene-state :t (+ t (:dt (:time api)))))))

   :render (fn [api _] ((:default-render api)))
   :cleanup (fn [api _] ((:clear-mesh! api)))})

;; ============================================
;; 4. 扫描网格展示
;; ============================================

(defn sweep-scene []
  {:init (fn [api]
           (let [{:keys [set-clear-color! set-mesh! clear-mesh!]} api]
             (set-clear-color! [0.02 0.02 0.05 1.0])
             ;; 初始弹簧
             (let [spring (sweep/make-spring 1.0 0.1 3 2.0 32 8)]
               (upload-faces-mesh! set-mesh! spring))
             {:t 0.0 :mode :spring}))

   :update (fn [api scene-state]
             (let [{:keys [set-mesh!]} api
                   {:keys [t mode]} scene-state
                   dt (:dt (:time api))
                   new-t (+ t dt)]
               (when (> new-t 0.5)
                 (let [new-mode (case mode
                                  :spring :pipe
                                  :pipe :vase
                                  :vase :spring)
                       result (case new-mode
                                :spring (sweep/make-spring 1.0 0.1 3 2.0 32 8)
                                :pipe (let [path (curves/make-helix-points
                                                  1.0 3.0 3 50)]
                                        (sweep/make-pipe path 0.2 8))
                                :vase (sweep/make-revolution
                                       [[0.3 0.0 0.0] [0.5 0.5 0.0]
                                        [0.4 1.0 0.0] [0.6 1.5 0.0]]
                                       32))]
                   (upload-faces-mesh! set-mesh! result)
                   (assoc scene-state :t 0.0 :mode new-mode)))
               (assoc scene-state :t new-t)))

   :render (fn [api _] ((:default-render api)))
   :cleanup (fn [api _] ((:clear-mesh! api)))})

;; ============================================
;; 5. 自然模拟展示
;; ============================================

(defn nature-scene []
  {:init (fn [api]
           (let [{:keys [set-clear-color! set-cubes! clear-cubes!
                         set-line-segments! clear-line-segments!]} api]
             (set-clear-color! [0.02 0.03 0.02 1.0])
             (clear-cubes!)
             ;; L-System 植物
             (let [fern-paths (lsys/lsystem-to-paths (lsys/make-fern))]
               (set-line-segments!
                (paths->line-segments fern-paths [0.2 0.9 0.3])))
             ;; Boid 系统
             {:boid-system (boid/make-boid-system
                            50
                            {:min [-3.0 -2.0 -3.0] :max [3.0 2.0 3.0]})
              :t 0.0}))

   :update (fn [api scene-state]
             (let [{:keys [set-cubes!]} api
                   {:keys [boid-system t]} scene-state
                   dt (:dt (:time api))
                   new-t (+ t dt)
                   ;; 更新 Boid
                   updated-system (boid/update-boid-system boid-system dt)
                   positions (boid/boid-system-positions updated-system)]
               ;; 用立方体表示 Boid
               (set-cubes! (map-indexed
                            (fn [idx pos]
                              {:id idx
                               :pos pos
                               :scale [0.05 0.05 0.05]
                               :color [0.9 0.7 0.2]})
                            positions))
               (assoc scene-state
                      :boid-system updated-system
                      :t new-t)))

   :render (fn [api _] ((:default-render api)))

   :cleanup (fn [api _]
              ((:clear-cubes! api))
              ((:clear-line-segments! api)))})

;; ============================================
;; 场景注册
;; ============================================

(defn all-showcase-scenes []
  {:showcase-basics (basics-scene)
   :showcase-curves (curves-scene)
   :showcase-subdiv (subdiv-scene)
   :showcase-sweep (sweep-scene)
   :showcase-nature (nature-scene)})

;; ============================================
;; 运行展示
;; ============================================

(defn run-demo
  "运行单个展示"
  [scene-key]
  (let [scenes (all-showcase-scenes)
        scene (get scenes scene-key)]
    (if scene
      (do (api/scene! scene-key)
          (println (str "Loaded demo: " scene-key)))
      (println (str "Unknown demo: " scene-key ". Available: " (keys scenes))))))

(defn run-all-demos
  "运行所有展示（时间线模式）"
  []
  (let [timeline [{:scene :showcase-basics :duration 10.0}
                  {:scene :showcase-curves :duration 12.0}
                  {:scene :showcase-subdiv :duration 12.0}
                  {:scene :showcase-sweep :duration 12.0}
                  {:scene :showcase-nature :duration 15.0}]]
    (api/timeline! timeline)
    (api/start-demo!)
    (println "Started demo timeline. Press 'q' to stop.")))

(defn list-demos
  "列出所有可用展示"
  []
  (println "Available demos:")
  (doseq [k (keys (all-showcase-scenes))]
    (println (str "  - " k))))
