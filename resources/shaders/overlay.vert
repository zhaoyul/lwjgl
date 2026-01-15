#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 aUV;

uniform vec2 uViewport;

out vec2 vUV;

void main() {
    vec2 ndc = vec2((aPos.x / uViewport.x) * 2.0 - 1.0,
                    1.0 - (aPos.y / uViewport.y) * 2.0);
    vUV = aUV;
    gl_Position = vec4(ndc, 0.0, 1.0);
}
