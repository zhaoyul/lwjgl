# Kons-9 对齐工作上下文

## 当前状态
- 已完成多面体细分/分型化、切角立方体/盒子、多点等值面、弹簧等值面、资源生长等场景。
- 已更新快捷键、默认时间线、文档示例。
- 已在本机验证 `clj -M:natives-intel:hotreload` 能启动并切换新场景（无编译错误）。

## 最新 nREPL 端口
- 最近一次启动日志显示：`nREPL listening on port 63183`
- 若换机重启，请用：`clj-nrepl-eval --discover-ports`

## 已新增/对齐场景
- `:resource-growth` 资源曲线驱动生长（高度场 + 曲线 + 立方体生长）
- `:polyhedron-refine` 多面体细分 + 线框叠加
- `:polyhedron-fractal` 多面体分型
- `:cut-cube` 切角立方体
- `:box` 盒子多面体
- `:isosurface-points` 点集等值面
- `:spring-isosurface` 弹簧网格等值面

## 快捷键新增
- `R` :polyhedron-refine
- `F` :polyhedron-fractal
- `C` :cut-cube
- `B` :box
- `T` :resource-growth
- `O` :isosurface-points
- `Y` :spring-isosurface

## 默认演示时间线已加入
`demo-timeline` 更新包含以上场景。

## 关键修改文件
- `src/lwjgl/experiment/kons9/scenes.clj`
  - 新增 `polyline->line-data`、新增场景定义
  - `:resource-growth` 使用 `dist-to-polyline` 在 XZ 平面计算资源距离
  - `:polyhedron-refine`/`:cut-cube` 等开启 wireframe 叠加
- `src/lwjgl/experiment/kons9/input.clj`
  - 新增快捷键映射（见上）
- `src/lwjgl/experiment/hotreload.clj`
  - `scene-api` 新增 `:set-wireframe-overlay!`
  - `demo-timeline` 扩展
- `src/lwjgl/experiment/kons9/api.clj`
  - 新增 `wireframe-overlay!`
- `src/lwjgl/experiment/kons9/sdf.clj`
  - 增加 `(declare marching-tetrahedra)` 解决前向引用编译问题
- `readme.org`
  - 新场景列表、快捷键、示例补全

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
