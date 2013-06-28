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

uniform float4x4 modelViewMatrix;

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
  float3 v = normalize(g_camPos - fragmentWorldPos);
  float3 n = fragmentNormal;
 
  if(dot(v,n) <= 0.0f)
    n *= (-1.0);
 
  float3 color = float3(0,0,0);
  
  for(int i=0;i<2;i++)
  {
    float3 l = normalize(g_lightPos[i] - fragmentWorldPos);
    float3 diffuseTerm  = EvalDiffuseBRDFTerm(l, n, g_matDiffuseColors[0]*fragmentFrontColor);
    float3 specularTerm = EvalSpecularBRDFTerm_Phong(v, l, n, g_matSpecularColors[0]*fragmentFrontColor, 100);

    color += 0.75f*(fragmentFrontColor*g_matAmbientColors[0] + diffuseTerm + specularTerm);
  }
 
  fragColor = float4(color,1.0f);
}








