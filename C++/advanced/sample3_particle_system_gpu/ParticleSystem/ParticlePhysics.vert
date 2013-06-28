#version 400

#define float2 vec2
#define float3 vec3
#define float4 vec4

#include "rand.frag"

in  float4 vertex;
in  float4 velocity;
in  float inRndSeed;

out float4 newPosAndSize;
out float4 newVelAndHp;
out float outRndSeed;

uniform float delta_t;

uniform float3 epicenter = float3(0,1,0);
uniform float3 grav = float3(0,-0.2,0);
uniform float3 wind = float3(0,0,0);

void main(void)
{
  float3 prevPos  = vertex.xyz;
  float  prevSize = vertex.w;
  
  float3 prevVel  = velocity.xyz;
  float  prevHp   = velocity.w;
  float  rndSeed  = inRndSeed;
  
  float3 resultingForce = grav + wind;
  
  float3 vel = prevVel + delta_t*resultingForce;
  float  hp  = prevHp  - delta_t;
  
  float3 pos = prevPos  + 20*delta_t*vel;
  float size = prevSize;
  
  if(hp <= 0.0f)
  {    
    vel  = 0.2f*nextRand3fSpherical(rndSeed); 
    pos  = epicenter; 
    hp   = 1.5f + 3.0f*nextRandf(rndSeed);
    size = 0.05f + 0.2f*nextRandf(rndSeed);
  }
  
  outRndSeed    = rndSeed;
  newVelAndHp   = float4(vel,hp);
  newPosAndSize = float4(pos,size);  
}
