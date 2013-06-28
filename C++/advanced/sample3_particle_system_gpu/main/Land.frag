#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float3 fragmentViewPos;
in float3 fragmentNormal;
in float2 fragmentTexCoord;

out float4 fragColor;

uniform float3 g_matAmbientColors[4];
uniform float3 g_matDiffuseColors[4];
uniform float3 g_matSpecularColors[4];

uniform float3 g_lightPos[2];
uniform float3 g_camPos;

uniform float4x4 modelViewMatrix;

uniform sampler2D diffuseTexture;
uniform sampler2D specularTexture;

float3 EvalDiffuseBRDFTerm(float3 l, float3 norm, float3 color)
{
  return color*max(dot(norm,l),0);
}

float3 EvalSpecularBRDFTerm_Phong(float3 v, float3 l, float3 norm, float3 color, float power)
{
  float3 r = reflect((-1)*l, norm);
  return color*pow(max(dot(r,v),0), power);
}


void main(void)
{	
  float3 v = normalize(-fragmentViewPos);
  float3 n = fragmentNormal;
 
  float3 color = float3(0,0,0);
  float3 diffuseColor  = texture(diffuseTexture, fragmentTexCoord).xyz;
  float3 specularColor = float3(0,0,0);//texture(specularTexture, fragmentTexCoord).xyz;
  
  for(int i=0;i<2;i++)
  {
    float3 l = normalize(g_lightPos[i] - fragmentViewPos);
    float3 diffuseTerm  = EvalDiffuseBRDFTerm(l, n, diffuseColor);
    float3 specularTerm = EvalSpecularBRDFTerm_Phong(v,l,n,specularColor,80);
    
    color += 0.5*diffuseTerm + 0.5f*specularTerm;
  }
  
  color += 0.05f*diffuseColor;
  
  //color = diffuseColor*0.75;
  //color = specularColor*0.75;
 
  fragColor = float4(color,1.0f);
}








