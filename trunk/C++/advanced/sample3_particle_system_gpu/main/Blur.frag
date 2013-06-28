#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float2 fragmentTexCoord;

out float4 fragColor;

uniform sampler2D colorTexture;

uniform int blurDirection;
uniform int width;
uniform int height;

const int SAMPLES_NUM = 10;
const int SAMPLE_LOD = 3;
const float STEP_MULT = 4.0f;  


void main(void)
{
  float2 dir = (blurDirection == 1) ? float2(0,1) : float2(1,0);	
  
  float2 stepXY = STEP_MULT*float2(1.0f/float(width), 1.0f/float(height)); 
  
  float3 color0 = textureLod(colorTexture, fragmentTexCoord, SAMPLE_LOD).xyz;
  float3 color1 = color0;
 
  float2 samplePos0 = fragmentTexCoord;
  float2 samplePos1 = fragmentTexCoord;
  
  float multiplyer = 1.0f;
  float summWt = 0.0f;
  for(int i=0;i<SAMPLES_NUM;i++)
  {
    samplePos0 += stepXY*dir;
    samplePos1 += stepXY*dir*(-1.0f);
    
    color0 += multiplyer*textureLod(colorTexture, samplePos0, SAMPLE_LOD).xyz;
    color1 += multiplyer*textureLod(colorTexture, samplePos1, SAMPLE_LOD).xyz;
    
    summWt += multiplyer;
    multiplyer *= 0.9f;
  }
  
  float3 color = (color0 + color1)/(2.0f*summWt);
  
	fragColor = float4(color, 1);
}

