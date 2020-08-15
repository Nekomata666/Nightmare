#version 450 core

layout(location = 0) in vec3 positions;
// layout(location = 1) in vec3 color;

// layout(location = 0) out vec3 fragment;

void main()
{
    gl_Position = vec4(positions, 1.0);
    // fragment = color;
}