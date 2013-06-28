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
layout (triangle_strip, max_vertices = 3) out;

in float3 vertWorldPos [];
in float3 vertFrontColor [];
in float2 vertTexCoord [];


out float3 fragmentWorldPos;
out float3 fragmentNormal; 
out float3 fragmentFrontColor;
out float2 fragmentTexCoord;

void main ()
{
  float3 A = vertWorldPos[0];
  float3 B = vertWorldPos[1];
  float3 C = vertWorldPos[2];
  
  float3 faceNormal = normalize(cross(A-B, A-C));
  faceNormal = (modelViewMatrix*float4(faceNormal,0)).xyz; // ?? 
 
  // optimize this mult, do it once, on the CPU, not for each triangle!
  //
  float4x4 modelViewProjectionMatrix = projectionMatrix*modelViewMatrix;
  
  gl_Position = modelViewProjectionMatrix*float4(A,1);
  fragmentFrontColor = vertFrontColor[0];
  fragmentTexCoord   = vertTexCoord[0];
  fragmentWorldPos   = A;
  fragmentNormal     = faceNormal;
  EmitVertex();

  gl_Position = modelViewProjectionMatrix*float4(B,1);
  fragmentFrontColor = vertFrontColor[1];
  fragmentTexCoord   = vertTexCoord[1];
  fragmentWorldPos   = B;
  fragmentNormal     = faceNormal;
  EmitVertex();

  gl_Position = modelViewProjectionMatrix*float4(C,1);
  fragmentFrontColor = vertFrontColor[2];
  fragmentTexCoord   = vertTexCoord[2];
  fragmentWorldPos   = C;
  fragmentNormal     = faceNormal;
  EmitVertex();
  
  EndPrimitive();
}
