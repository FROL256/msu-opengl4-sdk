#version 410 core

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float3 fragmentWorldPos;
in float3 fragmentNormal;
in float2 fragmentTexCoord;

out float4 fragColor;

uniform sampler2D diffuseTexture;

void main(void)
{	 
  fragColor = texture(diffuseTexture, fragmentTexCoord);
}


