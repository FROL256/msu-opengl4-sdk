#define CL_CC

#define GLOBAL_ID_X get_global_id(0)
#define GLOBAL_ID_Y get_global_id(1)

#define LOCAL_ID_X  get_local_id(0)
#define LOCAL_ID_Y  get_local_id(1)

inline float4 make_float4(float a, float b, float c, float d) { float4 res; res.x = a; res.y = b; res.z = c; res.w = d; return res; }

__kernel void GenerateSomeProceduralImage(__global float4* a_out, int w, int h, int index, float t) 
{
  const int x = get_global_id(0);
  const int y = get_global_id(1);

  float tx = (float)x/(float)w; // tex coord x
  float ty = (float)y/(float)h; // tex coord y

  const float PI = 3.14159265358979323846f;

  float t2 = t;

  if (tx <= 0.5f && ty <= 0.5f)
  {
    t2 = t;
  }
  else if (tx > 0.5f && ty <= 0.5f)
  {
    t2 = t + PI/4.0f;
  }
  else if (tx <= 0.5f && ty > 0.5f)
  {
    t2 = t + PI/2.0f;
  }
  else
  {
    t2 = t + 3.0f*PI / 4.0f;
  }

  a_out[y*w + x] = make_float4(sin(t2), cos(t2), cos(t), 0);
}
