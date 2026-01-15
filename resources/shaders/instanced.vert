#version 330 core

layout (location = 0) in vec2 aPos;
layout (location = 1) in vec2 iPos;
layout (location = 2) in vec2 iSize;

uniform float uTime;

out vec2 vPos;

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

float hash1(float n) {
    return fract(sin(n) * 43758.5453);
}

mat3 axisAngle(vec3 axis, float angle) {
    float c = cos(angle);
    float s = sin(angle);
    float t = 1.0 - c;
    return mat3(
        t * axis.x * axis.x + c,         t * axis.x * axis.y - s * axis.z, t * axis.x * axis.z + s * axis.y,
        t * axis.x * axis.y + s * axis.z, t * axis.y * axis.y + c,         t * axis.y * axis.z - s * axis.x,
        t * axis.x * axis.z - s * axis.y, t * axis.y * axis.z + s * axis.x, t * axis.z * axis.z + c
    );
}

void main() {
    vec3 local = vec3(aPos * iSize, 0.0);
    float seed = float(gl_InstanceID) + 1.0;
    float h0 = hash1(seed * 12.9898);
    float h1 = hash1(seed * 78.233);
    float h2 = hash1(seed * 45.164);
    vec3 axis = normalize(vec3(h0 * 2.0 - 1.0, h1 * 2.0 - 1.0, h2 * 2.0 - 1.0));
    float period = mix(30.0, 60.0, hash1(seed * 93.133));
    float phase = hash1(seed * 11.7) * 6.2831853;
    float dir = (hash1(seed * 17.3) < 0.5) ? -1.0 : 1.0;
    float angle = dir * (uTime * (6.2831853 / period) + phase);
    vec3 world = axisAngle(axis, angle) * local + vec3(iPos, 0.0);
    vPos = world.xy;
    gl_Position = vec4(world, 1.0);
}
