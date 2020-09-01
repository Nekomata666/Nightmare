#version 460 core

layout(binding = 0) uniform UniformBufferObject
{
    vec4 axis;
    vec4 conj;
} ubo;

layout(location = 0) in vec3 positions;
layout(location = 1) in vec3 color;

layout(location = 0) out vec3 fragment;


vec3 colors = vec3(1.0, 0.0, 1.0);

vec4 multiply(vec4 q1, vec4 q2)
{
    vec4 qr;
    qr.x = (q1.w * q2.x) + (q1.x * q2.w) + (q1.y * q2.z) - (q1.z * q2.y);
    qr.y = (q1.w * q2.y) - (q1.x * q2.z) + (q1.y * q2.w) + (q1.z * q2.x);
    qr.z = (q1.w * q2.z) + (q1.x * q2.y) - (q1.y * q2.x) + (q1.z * q2.w);
    qr.w = (q1.w * q2.w) - (q1.x * q2.x) - (q1.y * q2.y) - (q1.z * q2.z);
    return qr;
}

vec3 rotate(vec3 position)
{
    vec4 q_pos = vec4(position.x, position.y, position.z, 0);

    vec4 tmp = multiply(ubo.axis, q_pos);
    vec4 qr = multiply(tmp, ubo.conj);

    return vec3(qr.x, qr.y, qr.z);
}

void main()
{
    vec3 p = rotate(positions.xyz);
    gl_Position = vec4(p, 1.0);
    fragment = color;
}