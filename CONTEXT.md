# Kons-9 对齐工作上下文

## 当前状态
- 已完成多面体细分/分型化、切角立方体/盒子、多点等值面、弹簧等值面、资源生长等场景。
- 已更新快捷键、默认时间线、文档示例。
- 已在本机验证 `clj -M:natives-intel:hotreload` 能启动并切换新场景（无编译错误）。

## 最新 nREPL 端口
- 最近一次启动日志显示：`nREPL listening on port 63183`
- 若换机重启，请用：`clj-nrepl-eval --discover-ports`

## 已新增/对齐场景
- `:geometry`、`:rig`、`:sweep`、`:heightfield` 及其粒子/扫掠/动态变体已纳入默认列表，配套的清屏、立方体、网格等资源管理复刻自 `kons-9-main`。
- `:resource-growth`、`:polyhedron-refine`、`:polyhedron-fractal`、`:cut-cube`、`:box` 等 polyhedron 扩展场景实现了 wireframe 叠加、polyline 距离计算与分型参数。
- `:isosurface-points`、`:isosurface-curves`、`:isosurface-particles` 与 `:spring-isosurface` 共享新的 SDF 生成器接口（`isosurface-from-curves!`/`isosurface-from-points!`），可和弹簧/粒子数据实时耦合。
- `:spring`、`:particle-fields`、`:surface-particles`、`:polyhedron-particles`、`:polyhedron-vertices`、`:polyhedron-face-centers`、`:ecosystem` 等互动场景已同步 `kons-9-main` 的力场、精灵、生态、积分动画玩法。
- `:sdf-demo` / `:sweep-live` 进一步演示 SDF 原语（球/盒/圆环）组合、marching tetrahedra 生成 & 彩虹路径、以及可调参数的 sweep mesh（segments/twist/taper）+ 彩虹路径可视化，接近参考演示中“参数即刻更新”的 feel。

## 快捷键新增
- `R` :polyhedron-refine
- `F` :polyhedron-fractal
- `C` :cut-cube
- `B` :box
- `T` :resource-growth
- `O` :isosurface-points
- `Y` :spring-isosurface

## 快捷键与演示时间线
- `1-9` 切换基础场景，`H/J/K/L/U/I/M/N/R/F/C/B/T/O/Y` 加载 `kons9` 扩展，`0` 触发 `demo-timeline`、`P` 暂停/恢复、`M` 切换手动播放（空格播放）、`D` 重置当前场景。
- `demo-timeline` 按 `:geometry→:rig→:sweep→:heightfield→:resource-growth→:heightfield-sweep→:surface-particles→:polyhedron-refine→:polyhedron-fractal→:cut-cube→:box→:isosurface-points→:isosurface-curves→:isosurface-particles→:spring→:spring-isosurface→:ecosystem` 顺序滚动，场景时长控制在 6~10 秒，确保展示 `kons-9-main` 的完整节奏。
- `demo-timeline` 现在在 `:spring-isosurface` 后接 `:sdf-demo` 与 `:sweep-live`，再到 `:ecosystem`，将新的 SDF/UV sweep 演示纳入默认顺序。
- 所有快捷键和时间线通过 `src/lwjgl/experiment/kons9/input.clj`+`hotreload` 注册，便于在 nREPL 中动态补充新场景并保持 `kons-9-main` 的手感。

## API/DSL 与 live
- `src/lwjgl/experiment/kons9/api.clj` 暴露 `scene!`/`scene-list`/`timeline!`/`start-demo!`/`pause!` 等接口，并新增 `points!`、`mesh!`、`wireframe-overlay!`、`line-segments!`、`clear-*` 系列，和参考项目的 cheat-sheet 一致。
- `src/lwjgl/experiment/kons9/dsl.clj` 实现动画、振荡、关键帧、贝塞尔缓动等函数，可在 `kons9.live/live!` 中以 `:nodes`/`:build`/`:update` 自定义 `dsl/mesh`、`dsl/points`、`dsl/group`，直接复用 `kons-9-main` 的 Live 玩法。
- `src/lwjgl/experiment/kons9/live.clj` 提供 `:live` 场景，切换时自动清理立方体/点网格/rig，使 `kons9` actor 风格的实时构建流程能覆盖原始参考的 "live coding" 体验。

## 默认演示时间线已加入
`demo-timeline` 更新包含以上场景。

## 关键修改文件
- `src/lwjgl/experiment/kons9/math.clj`
-  - 添加 `rainbow-color` 便于根据位置/参数生成渐变颜色
- `src/lwjgl/experiment/kons9/sdf.clj`
-  - 增加球/盒/圆环 SDF、translate/union/intersect/difference，以及 `isosurface-from-sdf`
- `src/lwjgl/experiment/kons9/api.clj`
-  - 暴露新的 SDF helpers 和 mesh-from-SDF/isosurface 的快捷调用
- `src/lwjgl/experiment/kons9/geometry.clj`
-  - 为 `sweep-mesh` 加入可选 `:taper` 控制，方便动态调参
- `src/lwjgl/experiment/kons9/scenes.clj`
-  - 新增 `:sdf-demo` 与 `:sweep-live`，展示 SDF/UV sweep 即时更新
- `src/lwjgl/experiment/hotreload.clj`
-  - `demo-timeline` 扩展到 `:sdf-demo`、`:sweep-live`
- `CONTEXT.md`
-  - 补充新场景/SDF + 时间线说明

## 如何快速验证
```bash
clj -M:natives-intel:hotreload
clj-nrepl-eval --discover-ports
```
REPL 内：
```clojure
(require '[lwjgl.experiment.hotreload :as hr] :reload)
(require '[lwjgl.experiment.kons9.scenes :as kscenes] :reload)
(reset! hr/scenes {})
((var hr/ensure-default-scenes!))
(hr/set-scene! :polyhedron-refine)
```

## 注意事项
- 线框叠加由 `wireframe-overlay` 控制，场景退出时会关闭。
- 相机缩放支持滚轮（GLFW scroll 回调），旋转为鼠标拖拽。
- 若发现旧场景未更新，请重置场景：`(hr/reset-scene!)`。

## 未清理文件
- `hs_err_pid4975.log`（JVM crash log，非代码）
- `kons-9-main/`（原参考项目目录）

## 下一步可能对齐方向
- 粒子驱动 sweep mesh 的进一步 live 绑定（更接近 kons-9 现场演示）
- UV mesh / 彩虹顶点色演示强化
- sprites 深度排序、粒子可视化更多预设
- 持续把 `kons-9-main/doc` 中的场景说明、参数建议、快捷键表同步到 `readme.org` / `CONTEXT.md`，确保参考项目的用户手册与本地 README 保持一致。
