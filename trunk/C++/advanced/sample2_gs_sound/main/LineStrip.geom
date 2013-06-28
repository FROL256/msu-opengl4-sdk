#version 330 core

#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

uniform float4x4 projectionMatrix;
uniform float4x4 modelViewMatrix;

uniform float3 g_camPos;

layout (triangles) in;
layout (line_strip, max_vertices = 3) out;

in float3 vertWorldPos [];
in float3 vertFrontColor [];

out float3 fragmentWorldPos;
out float3 fragmentNormal; 
out float3 fragmentFrontColor;

void main ()
{
  float3 A = vertWorldPos[0];
  float3 B = vertWorldPos[1];
  float3 C = vertWorldPos[2];
  
  A += 0.75f*normalize(A);
  B += 0.75f*normalize(B);
  C += 0.75f*normalize(C);
  
  float3 colorA = 2.0f*(float3(1.25,1,1) - vertFrontColor[0]) + float3(0.25f,0.25f,0.25f);
  float3 colorB = 2.0f*(float3(1.25,1,1) - vertFrontColor[1]) + float3(0.25f,0.25f,0.25f);
  float3 colorC = 2.0f*(float3(1.25,1,1) - vertFrontColor[2]) + float3(0.25f,0.25f,0.25f);
  
  float3 faceNormal = normalize(cross(A-B, A-C));
  faceNormal = (modelViewMatrix*float4(faceNormal,0)).xyz; // ??
 
  gl_Position = projectionMatrix*(modelViewMatrix*float4(A,1));
  fragmentFrontColor = colorA;
  fragmentWorldPos = A;
  fragmentNormal = faceNormal;
  EmitVertex();

  gl_Position = projectionMatrix*(modelViewMatrix*float4(B,1));
  fragmentFrontColor = colorB;
  fragmentWorldPos = B;
  fragmentNormal = faceNormal;
  EmitVertex();
  
  gl_Position = projectionMatrix*(modelViewMatrix*float4(C,1));
  fragmentFrontColor = colorC;
  fragmentWorldPos = C;
  fragmentNormal = faceNormal;
  EmitVertex();
  
  EndPrimitive();
}


