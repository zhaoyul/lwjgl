# AGENTS.md

- HiDPI/Retina: always set the OpenGL viewport from framebuffer size, not window size.
- After `GL/createCapabilities`, call `core/init-viewport!` for the initial viewport.
- Use `GLFWFramebufferSizeCallbackI` to update the viewport on resize.
- If you map cursor positions to NDC, keep window size separately and update it via `GLFWWindowSizeCallbackI`.
