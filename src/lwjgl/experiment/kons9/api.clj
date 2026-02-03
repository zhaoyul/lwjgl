(ns lwjgl.experiment.kons9.api
  "面向 REPL 的 kons-9 风格接口封装."
  (:require [lwjgl.experiment.hotreload :as hot]
            [lwjgl.experiment.kons9.geometry :as kgeom]
            [lwjgl.experiment.kons9.sdf :as ksdf]
            [lwjgl.experiment.kons9.dsl :as kdsl]
            [lwjgl.experiment.kons9.live :as klive]))

(defn scene!
  "切换到指定场景."
  [scene-key]
  (hot/set-scene! scene-key))

(defn scene-list
  "返回已注册的场景列表."
  []
  (hot/list-scenes))

(defn current-scene
  "返回当前场景 key."
  []
  (hot/current-scene))

(defn timeline!
  "设置时间线. items 形如 {:scene :foo :duration 5.0} 的向量."
  [items]
  (hot/set-timeline! items))

(defn start-demo!
  "启动默认演示时间线."
  []
  (hot/start-demo!))

(defn stop-demo!
  "停止默认演示时间线."
  []
  (hot/stop-demo!))

(defn pause!
  "暂停场景更新."
  []
  (hot/pause!))

(defn resume!
  "恢复场景更新."
  []
  (hot/resume!))

(defn clear-color!
  "设置清屏颜色."
  [r g b a]
  (hot/set-clear-color! r g b a))

(defn axis-style!
  "设置坐标轴样式."
  [style]
  (hot/set-axis-style! style))

(defn grid-style!
  "设置网格样式."
  [style]
  (hot/set-grid-style! style))

(defn transition!
  "设置过场时长(秒)."
  [duration]
  (hot/set-transition! duration))

(defn trigger-transition!
  "触发过场遮罩."
  []
  (hot/trigger-transition!))

(defn cube!
  "添加一个立方体, 返回 id."
  [& {:as opts}]
  (apply hot/add-cube! (mapcat identity opts)))

(defn cube-update!
  "更新立方体属性."
  [id & {:as updates}]
  (apply hot/update-cube! id (mapcat identity updates)))

(defn cube-remove!
  "移除指定 id 的立方体."
  [id]
  (hot/remove-cube! id))

(defn cube-clear!
  "清空所有立方体."
  []
  (hot/remove-all-cubes!))

(defn cubes!
  "批量设置立方体, 返回新建的 id 列表."
  [cubes]
  (hot/remove-all-cubes!)
  (vec (map (fn [c] (apply hot/add-cube! (mapcat identity (dissoc c :id)))) cubes)))

(defn points!
  "设置点云数据."
  [points & {:keys [colors]}]
  (hot/set-point-cloud! points :colors colors))

(defn clear-points!
  "清空点云数据."
  []
  (hot/clear-point-cloud!))

(defn mesh!
  "设置通用网格数据."
  [vertices indices]
  (hot/set-mesh! vertices indices))

(defn clear-mesh!
  "清空通用网格."
  []
  (hot/clear-mesh!))

(defn mesh-style!
  "设置通用网格样式."
  [style]
  (hot/set-mesh-style! style))

(defn uv-sphere
  "生成 UV 球体网格."
  [lat-segs lon-segs]
  (kgeom/uv-sphere lat-segs lon-segs))

(defn sweep-mesh
  "生成扫掠网格."
  [& opts]
  (apply kgeom/sweep-mesh opts))

(defn line-curve
  "生成线段曲线点."
  [p1 p2 num-segments]
  (kgeom/line-points p1 p2 num-segments))

(defn rectangle-curve
  "生成矩形曲线点."
  [width height num-segments]
  (kgeom/rectangle-points width height num-segments))

(defn square-curve
  "生成正方形曲线点."
  [side num-segments]
  (kgeom/square-points side num-segments))

(defn circle-curve
  "生成圆形曲线点."
  [diameter num-segments]
  (kgeom/circle-points diameter num-segments))

(defn arc-curve
  "生成圆弧曲线点."
  [diameter start-angle end-angle num-segments]
  (kgeom/arc-points diameter start-angle end-angle num-segments))

(defn spiral-curve
  "生成螺旋曲线点."
  [start-diameter end-diameter axis-length num-loops num-segments]
  (kgeom/spiral-points start-diameter end-diameter axis-length num-loops num-segments))

(defn sine-curve
  "生成正弦曲线点."
  [period frequency x-scale y-scale num-segments]
  (kgeom/sine-curve-points period frequency x-scale y-scale num-segments))

(defn bezier-curve
  "生成三次贝塞尔曲线点."
  [cv0 cv1 cv2 cv3 num-segments]
  (kgeom/bezier-curve-points cv0 cv1 cv2 cv3 num-segments))

(defn butterfly-curve
  "生成蝴蝶曲线点."
  [num-segments]
  (kgeom/butterfly-curve-points num-segments))

(defn hypotrochoid-curve
  "生成内旋轮线曲线点."
  [fixed-r rolling-r dist num-segments]
  (kgeom/hypotrochoid-curve-points fixed-r rolling-r dist num-segments))

(defn epitrochoid-curve
  "生成外旋轮线曲线点."
  [fixed-r rolling-r dist num-segments]
  (kgeom/epitrochoid-curve-points fixed-r rolling-r dist num-segments))

(defn random-points
  "生成随机点云."
  [num bounds-lo bounds-hi]
  (kgeom/random-points num bounds-lo bounds-hi))

(defn grid-points
  "生成规则网格点云."
  [nx ny nz bounds-lo bounds-hi]
  (kgeom/grid-points nx ny nz bounds-lo bounds-hi))

(defn tetrahedron-mesh
  "生成四面体网格."
  [diameter]
  (kgeom/tetrahedron-mesh diameter))

(defn cube-mesh
  "生成立方体网格."
  [side]
  (kgeom/cube-mesh side))

(defn octahedron-mesh
  "生成八面体网格."
  [diameter]
  (kgeom/octahedron-mesh diameter))

(defn dodecahedron-mesh
  "生成十二面体网格."
  [diameter]
  (kgeom/dodecahedron-mesh diameter))

(defn icosahedron-mesh
  "生成二十面体网格."
  [diameter]
  (kgeom/icosahedron-mesh diameter))

(defn mesh-from-sdf!
  "根据 SDF 函数生成网格并设置."
  [f opts]
  (let [{:keys [vertices indices]} (ksdf/marching-tetrahedra f opts)]
    (hot/set-mesh! vertices indices)))

(defn metaballs!
  "生成 metaballs 网格并设置."
  [& {:keys [min max res t]
      :or {min [-1.4 -1.1 -1.1]
           max [1.4 1.1 1.1]
           res 36
           t 0.0}}]
  (mesh-from-sdf! ksdf/sdf-metaballs {:min min :max max :res res :t t}))

(defn sphere-mesh!
  "生成 UV 球体网格并设置."
  [lat-segs lon-segs]
  (let [{:keys [vertices indices]} (kgeom/uv-sphere lat-segs lon-segs)]
    (hot/set-mesh! vertices indices)))

(defn sweep!
  "生成扫掠网格并设置."
  [& opts]
  (let [{:keys [vertices indices]} (apply kgeom/sweep-mesh opts)]
    (hot/set-mesh! vertices indices)))

(defn cube-node
  "创建立方体节点."
  [& {:as opts}]
  (apply kdsl/cube (mapcat identity opts)))

(defn points-node
  "创建点云节点."
  [& {:as opts}]
  (apply kdsl/points (mapcat identity opts)))

(defn mesh-node
  "创建网格节点."
  [& {:as opts}]
  (apply kdsl/mesh (mapcat identity opts)))

(defn group-node
  "创建分组节点."
  [& {:as opts}]
  (apply kdsl/group (mapcat identity opts)))

(defn anim
  "创建随时间变化的插值函数."
  [& {:as opts}]
  (apply kdsl/anim (mapcat identity opts)))

(defn bezier-ease
  "创建贝塞尔缓动函数."
  [p1 p2]
  (kdsl/bezier-ease p1 p2))

(defn osc
  "创建正弦振荡函数."
  [& {:as opts}]
  (apply kdsl/osc (mapcat identity opts)))

(defn keyframes
  "创建关键帧插值函数."
  [frames & {:as opts}]
  (apply kdsl/keyframes (concat [frames] (mapcat identity opts))))

(defn live!
  "启动 kons-9 风格 live 场景."
  [opts]
  (klive/live! opts))
