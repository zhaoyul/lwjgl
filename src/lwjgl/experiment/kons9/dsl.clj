(ns lwjgl.experiment.kons9.dsl
  "kons-9 风格的轻量场景 DSL。"
  (:require [lwjgl.experiment.kons9.math :as kmath])
  (:import (org.joml Matrix3f Matrix4f Vector3f)))

(defn- eval-value
  "按时间上下文求值。支持函数与向量。"
  [v ctx]
  (cond
    (fn? v) (v ctx)
    (vector? v) (mapv #(eval-value % ctx) v)
    :else v))

(defn- v+
  [a b]
  (if (vector? a)
    (mapv + a b)
    (+ a b)))

(defn- v-
  [a b]
  (if (vector? a)
    (mapv - a b)
    (- a b)))

(defn- v*
  [a s]
  (if (vector? a)
    (mapv #(* % s) a)
    (* a s)))

(defn- lerp
  "线性插值。"
  [a b t]
  (cond
    (and (number? a) (number? b)) (+ a (* (- b a) t))
    (and (vector? a) (vector? b)) (mapv #(lerp %1 %2 t) a b)
    :else b))

(defn- catmull-rom
  "Catmull-Rom 曲线插值。"
  [p0 p1 p2 p3 t]
  (let [t2 (* t t)
        t3 (* t t t)
        a (v* p1 2.0)
        b (v* (v- p2 p0) t)
        c (v* (v+ (v* p0 2.0)
                  (v* p1 -5.0)
                  (v* p2 4.0)
                  (v* p3 -1.0)) t2)
        d (v* (v+ (v* p0 -1.0)
                  (v* p1 3.0)
                  (v* p2 -3.0)
                  p3) t3)]
    (v* (v+ a b c d) 0.5)))

(defn- bezier-point
  [t p0 p1 p2 p3]
  (let [u (- 1.0 t)
        tt (* t t)
        uu (* u u)
        uuu (* uu u)
        ttt (* tt t)
        p (v* p0 uuu)
        p (v+ p (v* p1 (* 3.0 uu t)))
        p (v+ p (v* p2 (* 3.0 u tt)))
        p (v+ p (v* p3 ttt))]
    p))

(defn bezier-ease
  "创建贝塞尔缓动函数，p1/p2 为控制点。"
  [p1 p2]
  (fn [x]
    (let [x (kmath/clamp01 x)
          p0 [0.0 0.0]
          p3 [1.0 1.0]]
      (loop [lo 0.0
             hi 1.0
             i 0]
        (let [t (/ (+ lo hi) 2.0)
              [bx by] (bezier-point t p0 p1 p2 p3)]
          (if (>= i 24)
            by
            (if (< bx x)
              (recur t hi (inc i))
              (recur lo t (inc i)))))))))

(defn- ease-fn
  "获取缓动函数。"
  [ease]
  (cond
    (fn? ease) ease
    (= ease :smoothstep) (fn [t] (* t t (- 3.0 (* 2.0 t))))
    (= ease :ease-in) (fn [t] (* t t))
    (= ease :ease-out) (fn [t] (- 1.0 (* (- 1.0 t) (- 1.0 t))))
    (= ease :ease-in-out) (fn [t] (if (< t 0.5)
                                    (* 2.0 t t)
                                    (- 1.0 (* 2.0 (- 1.0 t) (- 1.0 t)))))
    (= ease :linear) (fn [t] t)
    (and (vector? ease) (= :bezier (first ease)))
    (bezier-ease (second ease) (nth ease 2))
    (and (map? ease) (= :bezier (:type ease)))
    (bezier-ease (:p1 ease) (:p2 ease))
    :else (fn [t] t)))

(defn anim
  "返回随时间变化的函数。支持数值与向量插值。
  参数：
    :from 起始值
    :to 结束值
    :duration 时长
    :offset 起始偏移
    :loop? 是否循环
    :ease 缓动类型
    :time-key 使用 :t 或 :since"
  [& {:keys [from to duration offset loop? ease time-key]
      :or {duration 1.0 offset 0.0 loop? true ease :linear time-key :t}}]
  (let [ease-f (ease-fn ease)]
    (fn [ctx]
      (let [t (double (or (get ctx time-key) 0.0))
            t (- t (double offset))
            duration (max 1.0e-6 (double duration))
            t (if loop?
                (mod t duration)
                (min duration (max 0.0 t)))
            u (kmath/clamp01 (/ t duration))
            u (ease-f u)]
        (lerp from to u)))))

(defn osc
  "返回正弦振荡函数。
  参数：
    :amp 幅度
    :freq 频率
    :phase 相位
    :offset 偏移
    :time-key 使用 :t 或 :since"
  [& {:keys [amp freq phase offset time-key]
      :or {amp 1.0 freq 1.0 phase 0.0 offset 0.0 time-key :t}}]
  (fn [ctx]
    (let [t (double (or (get ctx time-key) 0.0))
          v (Math/sin (+ (* 2.0 Math/PI freq t) phase))]
      (+ offset (* amp v)))))

(defn keyframes
  "按关键帧插值。
  参数：
    frames 为 [[t value] ...] 或 {:t :value} 结构
    :loop? 是否循环
    :time-key 使用 :t 或 :since
    :interp 插值方式（:linear/:step/:hold/:smoothstep/:catmull）
    :ease 缓动类型"
  [frames & {:keys [loop? time-key interp ease]
             :or {loop? false time-key :t interp :linear}}]
  (let [frames (mapv (fn [f]
                       (cond
                         (map? f) {:t (:t f)
                                   :value (:value f)
                                   :interp (:interp f)
                                   :ease (:ease f)}
                         (vector? f) {:t (nth f 0)
                                      :value (nth f 1)}
                         :else {:t 0.0 :value f}))
                     frames)
        frames (vec (sort-by :t frames))
        times (mapv :t frames)
        t-min (first times)
        t-max (last times)
        ease-default (ease-fn ease)]
    (fn [ctx]
      (let [t0 (double (or (get ctx time-key) 0.0))
            t (if loop?
                (let [span (max 1.0e-6 (- t-max t-min))]
                  (+ t-min (mod (- t0 t-min) span)))
                t0)
            f0 (first frames)
            fN (last frames)]
        (cond
          (<= t (:t f0)) (:value f0)
          (>= t (:t fN)) (:value fN)
          :else
          (let [idx (loop [i 0]
                      (if (>= (nth times (inc i)) t)
                        i
                        (recur (inc i))))
                fa (nth frames idx)
                fb (nth frames (inc idx))
                ta (:t fa)
                tb (:t fb)
                v-a (:value fa)
                v-b (:value fb)
                interp-type (or (:interp fa) interp)
                ease-f (or (when-let [e (:ease fa)] (ease-fn e)) ease-default)
                u (/ (- t ta) (- tb ta))
                u (kmath/clamp01 u)
                u (if (and ease-f (#{:linear :smoothstep} interp-type))
                    (ease-f u)
                    u)]
            (case interp-type
              :step (if (< u 1.0) v-a v-b)
              :hold v-a
              :smoothstep (lerp v-a v-b ((ease-fn :smoothstep) u))
              :catmull (let [p0 (:value (nth frames (max 0 (dec idx))))
                             p1 v-a
                             p2 v-b
                             p3 (:value (nth frames (min (dec (count frames)) (+ idx 2))))]
                         (catmull-rom p0 p1 p2 p3 u))
              (lerp v-a v-b u))))))))

(defn cube
  "创建立方体节点。"
  [& {:keys [pos rot scale color]
      :or {pos [0.0 0.0 0.0]
           rot [0.0 0.0 0.0]
           scale [1.0 1.0 1.0]
           color [0.8 0.8 0.8]}}]
  {:type :cube
   :pos pos
   :rot rot
   :scale scale
   :color color})

(defn points
  "创建点云节点。"
  [& {:keys [points colors]
      :or {points [] colors []}}]
  {:type :points
   :points points
   :colors colors})

(defn mesh
  "创建网格节点。"
  [& {:keys [mesh style]
      :or {style {:pos [0.0 0.0 0.0]
                  :rot [0.0 0.0 0.0]
                  :scale [1.0 1.0 1.0]
                  :color [0.7 0.7 0.9]}}}]
  {:type :mesh
   :mesh mesh
   :style style})

(defn group
  "创建分组节点。"
  [& {:keys [pos rot scale children]
      :or {pos [0.0 0.0 0.0]
           rot [0.0 0.0 0.0]
           scale [1.0 1.0 1.0]
           children []}}]
  {:type :group
   :pos pos
   :rot rot
   :scale scale
   :children children})

(defn- compose-matrix
  [parent pos rot scale]
  (if parent
    (kmath/chain-transform parent pos rot scale)
    (kmath/compose-transform pos rot scale)))

(defn- transform-point
  [^Matrix4f m [x y z]]
  (let [v (Vector3f. (float x) (float y) (float z))]
    (.transformPosition m v)
    [(.x v) (.y v) (.z v)]))

(defn- transform-mesh
  [mesh ^Matrix4f mat]
  (let [vertices ^floats (:vertices mesh)
        count (alength vertices)
        out (float-array count)
        normal-mat (doto (Matrix3f. mat)
                     (.invert)
                     (.transpose))]
    (loop [i 0]
      (when (< i count)
        (let [x (aget vertices i)
              y (aget vertices (+ i 1))
              z (aget vertices (+ i 2))
              nx (aget vertices (+ i 3))
              ny (aget vertices (+ i 4))
              nz (aget vertices (+ i 5))
              pos (Vector3f. x y z)
              nrm (Vector3f. nx ny nz)]
          (.transformPosition mat pos)
          (.transform normal-mat nrm)
          (aset-float out i (.x pos))
          (aset-float out (+ i 1) (.y pos))
          (aset-float out (+ i 2) (.z pos))
          (aset-float out (+ i 3) (.x nrm))
          (aset-float out (+ i 4) (.y nrm))
          (aset-float out (+ i 5) (.z nrm))
          (recur (+ i 6)))))
    (assoc mesh :vertices out)))

(defn- resolve-cube
  [node ctx parent-mat]
  (let [pos (or (eval-value (:pos node) ctx) [0.0 0.0 0.0])
        rot (or (eval-value (:rot node) ctx) [0.0 0.0 0.0])
        scale (or (eval-value (:scale node) ctx) [1.0 1.0 1.0])
        color (or (eval-value (:color node) ctx) [0.8 0.8 0.8])
        model (compose-matrix parent-mat pos rot scale)]
    {:model model :color color}))

(defn- resolve-points
  [node ctx parent-mat]
  (let [points (vec (eval-value (:points node) ctx))
        colors (vec (or (eval-value (:colors node) ctx) []))
        points (if parent-mat
                 (mapv #(transform-point parent-mat %) points)
                 points)]
    {:points points :colors colors}))

(defn- resolve-mesh
  [node ctx parent-mat]
  (let [mesh (eval-value (:mesh node) ctx)
        style (eval-value (:style node) ctx)]
    (when mesh
      (let [{:keys [pos rot scale color]} style
            pos (or pos [0.0 0.0 0.0])
            rot (or rot [0.0 0.0 0.0])
            scale (or scale [1.0 1.0 1.0])
            local-mat (compose-matrix parent-mat pos rot scale)
            mesh (if (or parent-mat (not= [0.0 0.0 0.0] pos) (not= [0.0 0.0 0.0] rot) (not= [1.0 1.0 1.0] scale))
                   (transform-mesh mesh local-mat)
                   mesh)
            style {:pos [0.0 0.0 0.0]
                   :rot [0.0 0.0 0.0]
                   :scale [1.0 1.0 1.0]
                   :color (or color [0.7 0.7 0.9])}]
        {:mesh mesh :style style}))))

(defn- merge-points
  [a b]
  (if (and a b)
    {:points (vec (concat (:points a) (:points b)))
     :colors (vec (concat (:colors a) (:colors b)))}
    (or a b)))

(defn- resolve-node
  [node ctx parent-mat acc]
  (let [type (:type node)]
    (case type
      :group
      (let [pos (or (eval-value (:pos node) ctx) [0.0 0.0 0.0])
            rot (or (eval-value (:rot node) ctx) [0.0 0.0 0.0])
            scale (or (eval-value (:scale node) ctx) [1.0 1.0 1.0])
            mat (compose-matrix parent-mat pos rot scale)
            children (or (eval-value (:children node) ctx) [])]
        (reduce (fn [acc child] (resolve-node child ctx mat acc)) acc children))

      :cube
      (update acc :instances conj (resolve-cube node ctx parent-mat))

      :points
      (update acc :points merge-points (resolve-points node ctx parent-mat))

      :mesh
      (assoc acc :mesh (resolve-mesh node ctx parent-mat))

      acc)))

(defn evaluate
  "计算节点集合，返回 {:instances :points :mesh}。"
  [nodes ctx]
  (reduce (fn [acc node] (resolve-node node ctx nil acc))
          {:instances [] :points nil :mesh nil}
          nodes))

(defn apply!
  "将 DSL 结果应用到渲染接口。"
  [engine {:keys [instances points mesh]}]
  (if (seq instances)
    ((:set-rig-segments! engine) instances)
    ((:clear-rig! engine)))
  (if (and points (seq (:points points)))
    ((:set-point-cloud! engine) (:points points) :colors (:colors points))
    ((:clear-point-cloud! engine)))
  (if mesh
    (let [{:keys [mesh style]} mesh
          {:keys [vertices indices]} mesh]
      ((:set-mesh-style! engine) style)
      ((:set-mesh! engine) vertices indices))
    ((:clear-mesh! engine))))
