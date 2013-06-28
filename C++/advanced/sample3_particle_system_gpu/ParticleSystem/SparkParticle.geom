#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4

layout (points) in;
layout (line_strip, max_vertices = 2) out;

in  float3 pointPos [];
in  float3 pointVelocity [];

out float2 fragmentTexCoord;
out float3 sparkColor;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

uniform float delta_t = 0.1f;

void main(void)
{ 
  float3 pos = pointPos[0];
  float3 vel = pointVelocity[0]; 
  
  float3 pos1 = pos - 0.75f*vel;
   
  gl_Position = projectionMatrix*(modelViewMatrix*float4(pos,1));
  sparkColor  = 2*float3(2,1,0);
  EmitVertex();

  gl_Position = projectionMatrix*(modelViewMatrix*float4(pos1,1));
  sparkColor  = 2*float3(1,2,0);
  EmitVertex();

  EndPrimitive();  
}


