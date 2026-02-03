(ns lwjgl.experiment.kons9.runtime)

(defn register-scene!
  "注册场景。"
  [scenes scene-key scene]
  (swap! scenes assoc scene-key scene)
  scene-key)

(defn list-scenes
  "返回已注册的场景 key 列表。"
  [scenes]
  (vec (keys @scenes)))

(defn current-scene
  "返回当前场景 key。"
  [app]
  (get-in @app [:scene :key]))

(defn current-scene-def
  "返回当前场景的定义。"
  [app scenes]
  (get @scenes (get-in @app [:scene :key])))

(defn apply-scene!
  "切换场景并执行清理与初始化。"
  [app scenes context-fn scene-key]
  (let [scene (get @scenes scene-key)]
    (when-not scene
      (throw (ex-info "未找到场景" {:scene scene-key})))
    (let [{:keys [key state]} (:scene @app)
          old-scene (get @scenes key)
          ctx (context-fn)]
      (when-let [cleanup (:cleanup old-scene)]
        (cleanup ctx state))
      (swap! app assoc :scene {:key scene-key :state nil :since 0.0})
      (let [init (:init scene)
            init-ctx (context-fn)
            new-state (when init (init init-ctx))]
        (swap! app assoc-in [:scene :state] new-state))
      scene-key)))

(defn update-scene!
  "更新当前场景。"
  [app scenes context-fn dt]
  (swap! app update-in [:scene :since] + dt)
  (when-not (get-in @app [:flags :paused?])
    (when-let [update-fn (:update (current-scene-def app scenes))]
      (let [ctx (context-fn)
            scene-state (get-in @app [:scene :state])
            new-state (update-fn ctx scene-state)]
        (swap! app assoc-in [:scene :state] new-state)))))

(defn update-timeline!
  "推进时间线。"
  [app scenes context-fn dt]
  (let [{:keys [enabled? items index elapsed]} (:timeline @app)]
    (when (and enabled? (seq items))
      (let [elapsed (+ elapsed dt)
            {:keys [duration]} (nth items index)]
        (if (>= elapsed duration)
          (let [next-index (mod (inc index) (count items))
                next-scene (:scene (nth items next-index))]
            (swap! app assoc-in [:timeline :index] next-index)
            (swap! app assoc-in [:timeline :elapsed] 0.0)
            (apply-scene! app scenes context-fn next-scene)
            (swap! app assoc :transition {:active? true
                                          :alpha 1.0
                                          :duration (get-in @app [:transition :duration])}))
          (swap! app assoc-in [:timeline :elapsed] elapsed))))))

(defn update-transition!
  "更新过场动画。"
  [app dt]
  (let [{:keys [active? alpha duration]} (:transition @app)]
    (when active?
      (let [step (if (pos? duration) (/ dt duration) 1.0)
            alpha (max 0.0 (- alpha step))]
        (if (<= alpha 0.0)
          (swap! app assoc :transition {:active? false :alpha 0.0 :duration duration})
          (swap! app assoc-in [:transition :alpha] alpha))))))

(defn set-timeline!
  "设置时间线。items 形如 {:scene :foo :duration 5.0} 的向量。"
  [app items]
  (swap! app assoc :timeline {:enabled? false
                              :items (vec items)
                              :index 0
                              :elapsed 0.0})
  :ok)

(defn start-timeline!
  "启动时间线并切换到首个场景。"
  [app scenes context-fn]
  (let [items (get-in @app [:timeline :items])]
    (when (seq items)
      (swap! app assoc-in [:timeline :enabled?] true)
      (apply-scene! app scenes context-fn (:scene (first items))))))

(defn stop-timeline!
  "停止时间线推进。"
  [app]
  (swap! app assoc-in [:timeline :enabled?] false))

(defn set-transition!
  "设置过场时长（秒）。"
  [app duration]
  (swap! app assoc-in [:transition :duration] (max 0.0 (double duration)))
  :ok)

(defn trigger-transition!
  "手动触发一次过场遮罩。"
  [app]
  (swap! app assoc :transition {:active? true
                                :alpha 1.0
                                :duration (get-in @app [:transition :duration])})
  :ok)

(defn pause!
  "暂停场景更新。"
  [app]
  (swap! app assoc-in [:flags :paused?] true))

(defn resume!
  "恢复场景更新。"
  [app]
  (swap! app assoc-in [:flags :paused?] false))
