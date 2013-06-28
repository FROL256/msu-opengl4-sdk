#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float2 fragmentTexCoord;

out float4 fragColor;

uniform sampler2D bloomTexture;
uniform sampler2D colorTexture;

float3 ToneMapping(float3 color)
{  
  float maxColorInv = 1.0f/max(color.x, max(color.y, color.z));
  return (maxColorInv < 1.0f) ? color*maxColorInv : color;
}

void main(void)
{	
  float3 color = textureLod(colorTexture, fragmentTexCoord, 0).xyz; 
  float3 bloom = textureLod(bloomTexture, fragmentTexCoord, 0).xyz; 

  color = ToneMapping(0.75f*color) + ToneMapping(5.0f*bloom);

	fragColor = float4(color, 1);
}

