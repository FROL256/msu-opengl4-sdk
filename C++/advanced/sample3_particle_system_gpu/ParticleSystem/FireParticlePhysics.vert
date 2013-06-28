#version 400
#extension GL_EXT_gpu_shader4 : enable

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

uniform float timeScale = 0.5f;
uniform float delta_t;

uniform float3 epicenter = float3(0,0.1,0);
uniform float3 grav = float3(0,0.2,0);
uniform float3 wind = float3(0,0,0);

uniform samplerBuffer vertPosBuffer;

void main(void)
{
  float dt = timeScale*delta_t;
  
  //float4 vertex = texelFetchBuffer(vertPosBuffer, gl_VertexID);
  
  float3 prevPos  = vertex.xyz;
  float  prevSize = vertex.w;
  
  float3 prevVel  = velocity.xyz;
  float  prevHp   = velocity.w;
  float  rndSeed  = inRndSeed;
 
  const int N = 1; 
  float3 forceParticlesRes = float3(0,0,0);
  float signOfForce = 1.0f;
  
  
 /* if(gl_VertexID < N)
  {
    signOfForce = 0.0f; //(gl_VertexID%2 == 0) ? 1 : -1;
  } 
 
  for(int i=0;i<N;i++)
  {
    float4 data = texelFetchBuffer(vertPosBuffer, i);
    float3 diff = data.xyz - prevPos;
    float len   = length(diff);
    
    if(len > 1e-5f && i != gl_VertexID)
    {
      diff *= (1.0f/len);
      forceParticlesRes += signOfForce*diff*(1.0f/(1.0f+len + 0.1f*len*len));
    }
  }*/
  
  float3 fieldForce = 10.05f*forceParticlesRes;// - 0.5f*float3(prevPos.x, 0, prevPos.z);
  
  float3 resultingForce = grav + wind + fieldForce;
  
  float3 vel = prevVel + dt*resultingForce;
  float  hp  = prevHp  - dt;
  
  float3 pos = prevPos + 20*dt*vel;
  float size = prevSize;
  
  vel.x *= (1.0f + dt*0.25f*(2.0f*nextRandf(rndSeed)- 1.0f));
  vel.y *= (1.0f + dt*0.25f*(2.0f*nextRandf(rndSeed)- 1.0f));
  vel.z *= (1.0f + dt*0.25f*(2.0f*nextRandf(rndSeed)- 1.0f));
  
  if(hp <= 0.0f)
  {    
    vel  = 0.1f*nextRand3fCosine(rndSeed, float3(0,1,0), 1.0f) + float3(0, 0.1f,0); 
    
    float offsetX = 2.0f*nextRandf(rndSeed)- 1.0f;
    float offsetZ = 2.0f*nextRandf(rndSeed)- 1.0f;
    
    pos  = epicenter + 0.25f*float3(offsetX,0,offsetZ); 
    
    hp   = 0.25f + 1.0f*nextRandf(rndSeed);
    size = 0.05f + 0.15f*nextRandf(rndSeed);
  }
  
  outRndSeed    = rndSeed;
  newVelAndHp   = float4(vel,hp);
  newPosAndSize = float4(pos,size);  
}
