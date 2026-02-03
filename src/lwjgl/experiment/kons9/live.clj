(ns lwjgl.experiment.kons9.live
  (:require [lwjgl.experiment.hotreload :as hot]
            [lwjgl.experiment.kons9.dsl :as dsl]))

(defonce live-state
  (atom {:init nil
         :update nil
         :build nil
         :nodes []}))

(defn- dsl-context
  [ctx]
  (let [{:keys [time scene]} ctx]
    {:t (:now time)
     :dt (:dt time)
     :frame (:frame time)
     :since (:since scene)
     :ctx ctx}))

(defn- engine-api
  []
  {:set-rig-segments! hot/set-rig-segments!
   :clear-rig! hot/clear-rig!
   :set-point-cloud! hot/set-point-cloud!
   :clear-point-cloud! hot/clear-point-cloud!
   :set-mesh! hot/set-mesh!
   :clear-mesh! hot/clear-mesh!
   :set-mesh-style! hot/set-mesh-style!})

(defn- live-scene
  []
  {:init (fn [ctx]
           (when-let [init-fn (:init @live-state)]
             (init-fn (dsl-context ctx))))
   :update (fn [ctx scene-state]
             (let [ctx* (dsl-context ctx)
                   state (or scene-state {})
                   update-fn (:update @live-state)
                   next-state (if update-fn (update-fn ctx* state) state)
                   build-fn (:build @live-state)
                   nodes (if build-fn
                           (build-fn ctx* next-state)
                           (:nodes @live-state))
                   result (dsl/evaluate nodes ctx*)]
               (dsl/apply! (engine-api) result)
               next-state))
   :render (fn [ctx _] (hot/render-default ctx))
   :cleanup (fn [_ _] nil)})

(defn ensure-live-scene!
  "确保 live 场景已注册。"
  []
  (when-not (some #{:live} (hot/list-scenes))
    (hot/register-scene! :live (live-scene))))

(defn live!
  "启动 live 场景。配置项：
  :init  初始化函数
  :update 每帧更新函数
  :build 生成节点函数
  :nodes 固定节点列表"
  [{:keys [init update build nodes]}]
  (swap! live-state merge {:init init :update update :build build :nodes (or nodes [])})
  (hot/remove-all-cubes!)
  (hot/clear-point-cloud!)
  (hot/clear-mesh!)
  (hot/clear-rig!)
  (ensure-live-scene!)
  (hot/set-scene! :live)
  :ok)
