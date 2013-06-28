#ifndef RND_GUARDIAN
#define RND_GUARDIAN

#ifndef float3
  #define float3 vec3
#endif

float nextRandf(inout float a_rnd)
{
  uint a_seed = floatBitsToUint(a_rnd);
  double a = 16807.0; //ie 7**5
  double m = 2147483647.0; //ie 2**31-1 
  double reciprocal_m = 4.656612875245796924105750827168e-10; // 1.0f/m

  double temp = double(a_seed) * a;
  a_seed = int(temp - m * floor(temp * reciprocal_m));
  a_rnd  = uintBitsToFloat(a_seed);
  return a_seed*float(reciprocal_m);
}

/*
float nextRandf(inout float a_rnd) // crappy random generator
{
  uint rnd = floatBitsToUint(a_rnd);
  
  // RNG with period 2^32 - 1, from:
  // G. Marsaglia Random Number Generators 
  // Journal of Modern Applied Statistical Methods, May 2003, Vol.2, No.2, стр.5
  rnd ^= rnd << uint(13); 
  rnd ^= rnd >> uint(17); 
  rnd ^= rnd << uint(5);

  a_rnd = clamp( float(rnd & uint(0x0000FFFF) )/65535.0f, 0.0f, 1.0f); 
	return a_rnd;
}
*/

float3 nextRand3f(inout float rndSeed) // [0..1]
{
  float3 rndVec;
  rndVec.x = nextRandf(rndSeed);
  rndVec.y = nextRandf(rndSeed);
  rndVec.z = nextRandf(rndSeed);
  return rndVec;
}

float3 nextRand3fSpherical(inout float rndSeed) // [-1..1]
{
  float r1 = nextRandf(rndSeed);
  float r2 = nextRandf(rndSeed);
    
  float phi = r1*3.141592654f* 2; //[0..2PI]
  float h   = r2*2 - 1; //[-1..1]

  float x = sin(phi)*sqrt(1-h*h); 
  float y = cos(phi)*sqrt(1-h*h); 
  float z = h;  
  
  return float3(x,y,z);
}


float3 nextRand3fCosine(inout float rndSeed, float3 direction, float power) // [-1..1]
{
  float r1 = nextRandf(rndSeed);
  float r2 = nextRandf(rndSeed);
    
  float phi = r1*3.141592654f* 2; //[0..2PI]
  float h   = r2*2 - 1; //[-1..1]

  float e = power;
  float cos_phi = cos(2*r1*3.141592654f);
  float sin_phi = sin(2*r1*3.141592654f);
  float cos_theta = pow(1.0f-r2,1.0f/(e+1.0f));
  float sin_theta = sqrt(1.0f-cos_theta*cos_theta);

  float3 deviation;
  deviation.x = sin_theta*cos_phi;
  deviation.y = sin_theta*sin_phi;
  deviation.z = cos_theta;

  float3 ny = direction;
  float3 nx = normalize(cross(ny, float3(1.04,2.93f,-0.6234f)));
  float3 nz = normalize(cross(nx, ny));
 
  float3 temp = ny;
  ny = nz;
  nz = temp;
  
  return nx*deviation.x + ny*deviation.y + nz*deviation.z;
}

#endif

