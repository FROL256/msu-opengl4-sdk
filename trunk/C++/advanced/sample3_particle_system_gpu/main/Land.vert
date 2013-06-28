#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

uniform float4x4 projectionMatrix;
uniform float4x4 modelViewMatrix;
uniform float4x4 objectMatrix;

in float4 vertex;
in float3 normal;
in float2 texCoord;

out float3 fragmentViewPos;
out float3 fragmentNormal;
out float2 fragmentTexCoord;

void main(void)
{
  float4 worldPos  = objectMatrix*vertex;
  float4 viewPos   = modelViewMatrix*worldPos;
  
  fragmentViewPos  = viewPos.xyz;
  fragmentNormal   = normal;
  fragmentTexCoord = texCoord;
  
  gl_Position = projectionMatrix*viewPos;
}
