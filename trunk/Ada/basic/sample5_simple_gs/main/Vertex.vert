#version 330

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;
uniform float g_elaspedTime = 1.0;

in vec4 vertex;

out vec3 vertWorldPos;
out vec3 vertFrontColor; 

void main(void)
{
  float l = length(vertex.xz)*sin(0.25f*g_elaspedTime);
  
  vertWorldPos   = vertex.xyz + 0.1*vec3(0,cos(l*25.0),0);
  vertFrontColor = vec3(1,1,1); // to create some morphing colored effects, not used in this sample
}
