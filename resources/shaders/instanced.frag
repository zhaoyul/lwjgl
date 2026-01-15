#version 330 core

in vec2 vLocal;
in vec3 vNormal;

out vec4 FragColor;

uniform vec3 uRectColor;

void main() {
    float grad = clamp(vLocal.y * 0.5 + 0.5, 0.0, 1.0);
    vec3 base = mix(uRectColor * 0.6, uRectColor * 1.2, grad);
    vec3 lightDir = normalize(vec3(0.6, -0.7, 0.7));
    float diff = max(dot(normalize(vNormal), lightDir), 0.0);
    vec3 lit = base * (0.2 + 0.8 * diff);
    FragColor = vec4(lit, 1.0);
}
