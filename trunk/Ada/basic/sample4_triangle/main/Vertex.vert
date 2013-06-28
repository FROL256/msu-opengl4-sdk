#version 330

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

in  vec2 vertex;

void main(void)
{
  vec4 viewPos = modelViewMatrix*vec4(vertex,0.0,1.0);
  gl_Position  = projectionMatrix*viewPos; //vec4(vertex, 0.0, 1.0);
}
