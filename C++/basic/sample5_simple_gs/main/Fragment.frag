#version 330

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float3 fragmentWorldPos;
in float3 fragmentNormal;
in float3 fragmentFrontColor;

out float4 fragColor;

uniform float3 g_matAmbientColors[4];
uniform float3 g_matDiffuseColors[4];
uniform float3 g_matSpecularColors[4];

uniform float3 g_lightPos[2];
uniform float3 g_camPos;

void main(void)
{	
  fragColor = float4(0,1,0,1);
}








