#version 330 core

in vec3 vLocal;
in vec3 vNormal;
flat in vec3 vColor;

out vec4 FragColor;

uniform vec3 uRectColor;
uniform float uUseGlobalColor;

void main() {
    vec3 baseColor = mix(vColor, uRectColor, uUseGlobalColor);
    float grad = clamp(vLocal.y * 0.5 + 0.5, 0.0, 1.0);
    vec3 base = mix(baseColor * 0.6, baseColor * 1.2, grad);
    vec3 lightDir = normalize(vec3(0.6, -0.7, 0.7));
    float diff = max(dot(normalize(vNormal), lightDir), 0.0);
    vec3 lit = base * (0.2 + 0.8 * diff);
    FragColor = vec4(lit, 1.0);
}
