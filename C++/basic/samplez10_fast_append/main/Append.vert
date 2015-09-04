#version 330

in  float vertex;
in  int   inMask;

out float  outDataFromVS;
out int    outMaskFromVS;

void main(void)
{
  outDataFromVS = vertex;
  outMaskFromVS = inMask;
}
