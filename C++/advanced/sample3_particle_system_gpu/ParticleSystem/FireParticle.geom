#version 400

uniform mat4 projectionMatrix;

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

in  vec4  pointViewPos [];
in  float pointSize    [];
in  float particleLive [];

out vec2  fragmentTexCoord;

void main(void)
{
  float size = pointSize[0];
  float t    = min(particleLive[0], 1.0f);
  
  float spriteNum  = 24.0f*t*t*t;
  int textureIndex = max(23 - int(spriteNum), 0);
  
  int y = textureIndex/6;
  int x = textureIndex - y*6;

  float wrapX = 1.0f/6.0001f;
  float wrapY = 1.0f/4.0001f;
  
  float startX = x*wrapX;
  float endX   = startX + wrapX;
  
  float startY = y*wrapY;
  float endY   = startY + wrapY;
   
  startY = 1.0f - startY;
  endY   = 1.0f - endY;  
    
  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(-size, -size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(startX, endY);
  EmitVertex();

  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(-size, size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(startX, startY);
  EmitVertex();

  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(size, -size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(endX, endY);
  EmitVertex();

  gl_Position      = projectionMatrix*(pointViewPos[0] + vec4(size, size, 0.0f, 0.0f));
  fragmentTexCoord = vec2(endX, startY);
  EmitVertex();

  EndPrimitive();  
}


