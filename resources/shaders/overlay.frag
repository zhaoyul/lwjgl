#version 330 core

in vec2 vUV;

out vec4 FragColor;

uniform sampler2D uText;
uniform vec3 uTextColor;

void main() {
    float a = texture(uText, vUV).r;
    FragColor = vec4(uTextColor, a);
}
