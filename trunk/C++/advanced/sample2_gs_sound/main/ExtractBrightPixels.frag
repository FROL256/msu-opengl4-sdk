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
const float BRIGHT_THRESHOLD = 0.5f;

float3 ToneMapping(float3 color)
{  
  float maxColorInv = 1.0f/max(color.x, max(color.y, color.z));
  return (maxColorInv < 1.0f) ? color*maxColorInv : color;
}

void main(void)
{	
  float3 color       = textureLod(colorTexture, fragmentTexCoord, 0).xyz;    // sample from the first mip map level
	float3 middleValue = textureLod(colorTexture, float2(0.5f, 0.5f), 10).xyz; // sample from the last mip map level 
	
	float middleLum = dot(middleValue, factor);	
	float lum = dot(color, factor);
	float bt  = BRIGHT_THRESHOLD;
	
	color = max(float3(0,0,0), color - float3(bt,bt,bt));
	
	if(lum > middleLum)
	  fragColor = float4(ToneMapping(color),1);
	else
	  fragColor = float4(0,0,0,1);
	  
}



