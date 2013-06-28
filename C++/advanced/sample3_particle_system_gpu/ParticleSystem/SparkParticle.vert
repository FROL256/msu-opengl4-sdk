#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4

in  float4 vertex;
in  float4 velocity;

out float3 pointPos;
out float3 pointVelocity;

void main(void)
{
  pointPos      = vertex.xyz;
  pointVelocity = velocity.xyz;
}

