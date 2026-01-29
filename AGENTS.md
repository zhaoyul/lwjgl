# AGENTS.md

## OpenGL 最佳实践

- HiDPI/Retina：始终使用 framebuffer 大小（而非窗口大小）设置 OpenGL viewport。
- 在调用 `GL/createCapabilities` 之后，调用 `core/init-viewport!` 初始化 viewport。
- 使用 `GLFWFramebufferSizeCallbackI` 在窗口大小变化时更新 viewport。
- 如果将光标位置映射到 NDC，请单独保存窗口大小，并通过 `GLFWWindowSizeCallbackI` 更新它。
- 当通过 `.get` 将 JOML 矩阵上传到 `FloatBuffer` 时，在调用 `glUniformMatrix4fv` 之前使用 `.rewind`（而不是 `.flip`），否则矩阵会上传为空，你只能看到清除颜色。

## 代码规范

- **所有代码注释必须使用中文**
- **所有函数 docstring 必须使用中文**
- 变量名可以使用英文，但复杂逻辑必须加中文注释说明

## 运行项目（macOS 架构支持）

本项目同时支持 Intel 和 Apple Silicon (ARM64) Mac。LWJGL 本地库是架构相关的。

### 方式一：使用 run.sh 脚本（推荐）

`run.sh` 脚本会自动检测你的架构：

```bash
# 运行带 nREPL 的 hotreload
./run.sh hotreload-nrepl

# 运行其他示例
./run.sh run
./run.sh cube-input
./run.sh model-loading

# 组合其他别名
./run.sh hotreload-nrepl cider-deps
```

### 方式二：手动选择别名

如果不使用脚本，请手动指定本地库别名：

**Intel Mac (x86_64)：**
```bash
clj -M:natives-intel:hotreload-nrepl
```

**Apple Silicon (M1/M2/M3 - ARM64)：**
```bash
clj -M:natives-arm:hotreload-nrepl
```

### 可用的架构别名

- `:natives-intel` - Intel macOS 本地库
- `:natives-arm` - Apple Silicon (ARM64) 本地库

在 macOS 上运行时，始终在其他别名之前包含其中之一。

## 代码示例规范

所有新增代码应遵循以下注释规范：

```clojure
;; 这是一个好的中文注释
(defn add-cube!
  "动态添加一个立方体。返回立方体的 ID。
  参数：
    :pos [x y z] - 位置（默认 [0 0 0]）
    :rot [rx ry rz] - 旋转角度（默认 [0 0 0]）
    :scale [sx sy sz] - 缩放系数（默认 [1 1 1]）
    :color [r g b] - 颜色 RGB 0-1（默认随机颜色）"
  [& {:keys [pos rot scale color]}]
  ;; 实现逻辑...
  )
```
