#version 330

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

uniform float4x4 projectionMatrix;
uniform float4x4 modelViewMatrix;

in float4 vertex;
in float3 normal;
in float2 texCoord;

out float3 vertWorldPos;
out float3 vertFrontColor;

uniform float timeParameter;
uniform float timeParameter2;
uniform float timeParameter3;
uniform float4x4 g_rotorMatrix;

void main(void)
{
  float4 vPosition = vertex;


  float fAngleParameter = timeParameter * 2.0 * 3.1415;
  float fAngleParameter2 = timeParameter2 * 2.0 * 3.1415;
  //float fAngleParameter3 = timeParameter3 * 2.0 * 3.1415;
  //fAngleParameter = fAngleParameter3;
  
  float fLength = length(vPosition.xyz);
  float fTheta  = acos(vPosition.z / fLength);
  float fPhi    = atan(vPosition.y, length(vPosition.xz));
  
  float fMultiplier = cos( 8.0 * fTheta*sin(fAngleParameter) + 3.6*fAngleParameter ) - sin( 6.0 * fPhi*sin(fAngleParameter) - 7.1*fAngleParameter );
  vPosition.xyz = vPosition.xyz * (0.8 + 0.4*fMultiplier);

  //float fMultiplier = cos( 8.0 * fTheta*sin(fAngleParameter) + 3.6*fAngleParameter ) - sin( 6.0 * fPhi*sin(fAngleParameter) - 7.1*fAngleParameter );
  //vPosition = (fMultiplier*g_rotorMatrix)*vPosition;

  vertWorldPos = vPosition.xyz;
  
  float colorMultiplier = fLength*sin(fAngleParameter);
  vertFrontColor = float3(1,1,1) * ( 0.7 + 0.3 * abs( sin( fTheta + fAngleParameter2 + fAngleParameter) ) ) + float3( 0.0, 0.0, 0.4 ) * colorMultiplier;
}



