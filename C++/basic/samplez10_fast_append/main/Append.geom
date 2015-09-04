#version 330

layout (points) in;
layout (points, max_vertices = 1) out;

in  float outDataFromVS [];
in  int   outMaskFromVS [];

out float  outData;

void main(void)
{ 
  float data = outDataFromVS[0];
  int   mask = outMaskFromVS[0]; 
  
  if(mask != 0)
  {
    outData = data;
    EmitVertex();
  }

  EndPrimitive();  
}

