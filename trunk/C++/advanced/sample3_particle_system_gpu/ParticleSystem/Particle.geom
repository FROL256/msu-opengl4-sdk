#version 400

uniform mat4 projectionMatrix;

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in  vec4  pointViewPos [];
in  float pointSize    [];

out vec2  fragmentTexCoord;

void main(void)
{
  float size = pointSize[0];
    
  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(-size, -size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(0.0f, 1.0f);
  EmitVertex();

  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(-size, size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(0.0f, 0.0f);
  EmitVertex();

  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(size, -size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(1.0f, 1.0f);
  EmitVertex();

  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(size, size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(1.0f, 0.0f);
  EmitVertex();

  EndPrimitive();  
}


