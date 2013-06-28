#version 330

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float2 fragmentTexCoord;

out float4 fragColor;

uniform sampler2D colorTexture;

const float3 factor = float3(0.27, 0.67, 0.06);

void main(void)
{	
  float3 color = textureLod(colorTexture, fragmentTexCoord, 0).xyz; // sample from the first mip map level
  
  float lum = dot(color, factor);

	fragColor = float4(lum,lum,lum, 1);
}

