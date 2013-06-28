#pragma once

#include <math.h>

struct float2
{
  float x,y;
};

struct float3
{
  float3():x(0), y(0), z(0){}
  float3(float a,float b, float c): x(a), y(b), z(c) {}

  float x,y,z;
};

struct float4
{
  float4():x(0), y(0), z(0){}
  float4(float a,float b, float c, float d): x(a), y(b), z(c), w(d) {}

  float x,y,z,w;
};




struct float4x4
{
  float4x4(){identity();} 

  void identity() 
  { 
    row[0] = float4(1,0,0,0);
    row[1] = float4(0,1,0,0);
    row[2] = float4(0,0,1,0);
    row[3] = float4(0,0,0,1);
  }

  float& M(int x, int y) {return ((float*)row)[y*4+x]; }
  float  M(int x, int y) const {return ((float*)row)[y*4+x]; }
  
  float* L() {return (float*)row;}
  const float* L() const {return (float*)row;}

  float4 row[4]; 
};


