#version 330

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float3 fragmentWorldPos;
in float3 fragmentNormal;
in float2 fragmentTexCoord;

out float4 fragColor[];


uniform float3 g_lightPos[2];
uniform float3 g_camPos;

uniform float3 g_diffuseColor  = float3(1,1,1);
uniform float3 g_specularColor = float3(1,1,1);

uniform sampler2D diffuseTexture;

float3 EvalDiffuseBRDFTerm(float3 l, float3 norm, float3 color)
{
  return color*max(dot(norm,l), 0.0f);
}


void main(void)
{	
  float3 v = normalize(-g_camPos);
  float3 n = fragmentNormal;
 
  float3 color = float3(0,0,0);
  
  float3 diffuseColor  = g_diffuseColor*texture(diffuseTexture, fragmentTexCoord).xyz;
  
  for(int i=0;i<2;i++)
  {
    float3 l = normalize(fragmentWorldPos - g_lightPos[i]); 
    
    float3 diffuseTerm  = EvalDiffuseBRDFTerm(l, n, diffuseColor);
    float3 specularTerm = float3(0,0,0);
    
    float dist = length(g_lightPos[i] - fragmentWorldPos);
    float attenuation = 1.0f/(1.0f + 0.005f*dist*dist);
    
    color += attenuation*(0.5*diffuseTerm + 0.5f*specularTerm);
  }
  
  color += 0.25f*diffuseColor; // ambient term
 
  fragColor[0] = float4(color,1.0f);
  fragColor[1] = float4(fragmentWorldPos,1.0f);
  fragColor[2] = float4(fragmentTexCoord,0.0f,1.0f);
}


