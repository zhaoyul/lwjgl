#version 330 core

in vec2 vPos;

out vec4 FragColor;

void main() {
    vec3 color = vec3(0.2 + 0.7 * abs(vPos.x),
                      0.25 + 0.6 * abs(vPos.y),
                      0.65);
    FragColor = vec4(color, 1.0);
}
