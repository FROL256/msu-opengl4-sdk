#version 330

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

uniform float4x4 projectionMatrix;
uniform float4x4 modelViewMatrix;

in float4 vertex;
in float3 normal;
in float2 texCoord;

out float3 vertWorldPos;
out float3 vertFrontColor;

uniform float timeParameter;

void main(void)
{
  vertWorldPos = vertex.xyz + float3(1,0,0)*sin(20*timeParameter-length(vertex.xyz-float3(0,0,1)));
  vertFrontColor = float3(1,1,1);
}



