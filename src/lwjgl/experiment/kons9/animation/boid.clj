(ns lwjgl.experiment.kons9.animation.boid
  "Boid 群体系统 - 对应 kons-9 的 plugins/boid-system.lisp"
  (:require [lwjgl.experiment.kons9.math :as math]))

;; ============================================
;; Boid 定义
;; ============================================

(defn make-boid
  "创建 Boid"
  [id pos & {:keys [max-speed max-force]
             :or {max-speed 2.0
                  max-force 0.1}}]
  {:id id
   :pos pos
   :vel [0.0 0.0 0.0]
   :accel [0.0 0.0 0.0]
   :max-speed max-speed
   :max-force max-force})

;; ============================================
;; 基础行为
;; ============================================

(defn- boid-separation-force
  "分离行为 - 避免碰撞"
  [boid neighbors desired-separation]
  (let [steer [0.0 0.0 0.0]
        count (atom 0)]
    (doseq [other neighbors]
      (let [d (math/dist (:pos boid) (:pos other))]
        (when (and (> d 0) (< d desired-separation))
          (let [diff (math/vsub (:pos boid) (:pos other))
                diff (math/normal diff)
                diff (math/vmul diff (/ 1.0 d))]
            (swap! count inc)
            (def steer (math/vadd steer diff))))))
    (if (> @count 0)
      (let [steer (math/vmul steer (/ 1.0 @count))
            steer (math/normal steer)
            steer (math/vmul steer (:max-speed boid))]
        (math/vsub steer (:vel boid)))
      [0.0 0.0 0.0])))

(defn- boid-alignment-force
  "对齐行为 - 速度一致"
  [boid neighbors]
  (let [sum-vel [0.0 0.0 0.0]
        count (atom 0)]
    (doseq [other neighbors]
      (swap! count inc)
      (def sum-vel (math/vadd sum-vel (:vel other))))
    (if (> @count 0)
      (let [avg-vel (math/vmul sum-vel (/ 1.0 @count))
            avg-vel (math/normal avg-vel)
            avg-vel (math/vmul avg-vel (:max-speed boid))]
        (math/vsub avg-vel (:vel boid)))
      [0.0 0.0 0.0])))

(defn- boid-cohesion-force
  "凝聚行为 - 向中心移动"
  [boid neighbors]
  (let [sum-pos [0.0 0.0 0.0]
        count (atom 0)]
    (doseq [other neighbors]
      (swap! count inc)
      (def sum-pos (math/vadd sum-pos (:pos other))))
    (if (> @count 0)
      (let [target (math/vmul sum-pos (/ 1.0 @count))]
        (math/vsub target (:pos boid)))
      [0.0 0.0 0.0])))

;; ============================================
;; Boid 系统
;; ============================================

(defn make-boid-system
  "创建 Boid 系统"
  [count bounds & {:keys [weights]
                   :or {weights {:separation 1.5
                                 :alignment 1.0
                                 :cohesion 1.0}}}]
  (let [boids (mapv (fn [i]
                      (make-boid i
                                 [(math/rand-range (first (:min bounds)) (first (:max bounds)))
                                  (math/rand-range (second (:min bounds)) (second (:max bounds)))
                                  (math/rand-range (nth (:min bounds) 2) (nth (:max bounds) 2))]))
                    (range count))]
    {:boids boids
     :weights weights
     :bounds bounds}))

(defn- get-neighbors
  "获取邻居"
  [boid all-boids perception-radius]
  (filterv #(and (not= (:id %) (:id boid))
                 (< (math/dist (:pos boid) (:pos %)) perception-radius))
           all-boids))

(defn update-boid
  "更新单个 Boid"
  [boid all-boids weights dt bounds]
  (let [neighbors (get-neighbors boid all-boids 5.0)
        ; 计算各种力
        sep (boid-separation-force boid neighbors 2.0)
        ali (boid-alignment-force boid neighbors)
        coh (boid-cohesion-force boid neighbors)
        ; 加权
        sep (math/vmul sep (:separation weights))
        ali (math/vmul ali (:alignment weights))
        coh (math/vmul coh (:cohesion weights))
        ; 合力
        accel (math/vadd sep ali coh)
        ; 限制加速度
        accel (if (> (math/len accel) (:max-force boid))
                (math/vmul (math/normal accel) (:max-force boid))
                accel)
        ; 更新速度
        new-vel (math/vadd (:vel boid) (math/vmul accel dt))
        new-vel (math/vmul new-vel 0.99)  ; 阻尼
        new-vel (if (> (math/len new-vel) (:max-speed boid))
                  (math/vmul (math/normal new-vel) (:max-speed boid))
                  new-vel)
        ; 更新位置
        new-pos (math/vadd (:pos boid) (math/vmul new-vel dt))
        ; 边界环绕
        new-pos [(math/wrap (first new-pos) (first (:min bounds)) (first (:max bounds)))
                 (math/wrap (second new-pos) (second (:min bounds)) (second (:max bounds)))
                 (math/wrap (nth new-pos 2) (nth (:min bounds) 2) (nth (:max bounds) 2))]]
    (assoc boid :pos new-pos :vel new-vel :accel accel)))

(defn update-boid-system
  "更新 Boid 系统"
  [system dt]
  (let [new-boids (mapv #(update-boid % (:boids system) (:weights system) dt (:bounds system))
                        (:boids system))]
    (assoc system :boids new-boids)))

;; ============================================
;; 输出
;; ============================================

(defn boid-system-positions
  "获取所有 Boid 位置"
  [system]
  (mapv :pos (:boids system)))
