#version 410 core

in  vec2 vertex;
out vec2 fragmentTexCoord;

void main(void)
{
  fragmentTexCoord = vertex * 0.5 + 0.5; 
  gl_Position = vec4(vertex, 0.0, 1.0);
}


