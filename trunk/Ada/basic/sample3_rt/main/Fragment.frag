#version 330

// just don't like these OGL's vec"n", cause in (DX,CUDA,Cg) they are float"n" 
//
#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3

in float2 fragmentTexCoord;

out vec4 fragColor;

uniform int g_screenWidth;
uniform int g_screenHeight;

uniform float3 g_bBoxMin = float3(-2,-2,-2);
uniform float3 g_bBoxMax = float3(2,2,2);

uniform float3 g_lightPos;

uniform float4x4 g_rayMatrix;

uniform float3 g_matAmbientColor;
uniform float3 g_matDiffuseColor;
uniform float3 g_matSpecularColor;

uniform float4 g_bgColor = float4(0,0,1,0);
uniform float3 g_pointLightColor;
uniform float3 g_environmentLightColor;

uniform float g_FractalPower = 10;

float3 EyeRayDir(float x, float y, float w, float h)
{
  float fov = 3.141592654f/(2.0f); 
  float3 ray_dir;
  
  ray_dir.x = x+0.5f - (w/2.0f);
  ray_dir.y = y+0.5f - (h/2.0f);
  ray_dir.z = -(w)/tan(fov/2.0f);
	
  return normalize(ray_dir);
}

bool RayBoxIntersection(float3 ray_pos, float3 ray_dir, float3 boxMin, float3 boxMax, inout float tmin, inout float tmax)
{
  ray_dir.x = 1.0f/ray_dir.x;
  ray_dir.y = 1.0f/ray_dir.y;
  ray_dir.z = 1.0f/ray_dir.z; 

  float lo = ray_dir.x*(boxMin.x - ray_pos.x);
  float hi = ray_dir.x*(boxMax.x - ray_pos.x);
  
  tmin = min(lo, hi);
  tmax = max(lo, hi);

  float lo1 = ray_dir.y*(boxMin.y - ray_pos.y);
  float hi1 = ray_dir.y*(boxMax.y - ray_pos.y);

  tmin = max(tmin, min(lo1, hi1));
  tmax = min(tmax, max(lo1, hi1));

  float lo2 = ray_dir.z*(boxMin.z - ray_pos.z);
  float hi2 = ray_dir.z*(boxMax.z - ray_pos.z);

  tmin = max(tmin, min(lo2, hi2));
  tmax = min(tmax, max(lo2, hi2));
  
  return (tmin <= tmax) && (tmax > 0.f);
}

//////////////////// Mandelbulb ////////////////////////////////////////

void powN(inout float3 z, float zr0, inout float dr)
{
    float power = g_FractalPower;

    float zo0 = asin(z.z / zr0);
    float zi0 = atan(z.y, z.x);
    float zr = pow(zr0, power - 1.0);
    float zo = (zo0) * power;
    float zi = (zi0) * power;
    float czo = cos(zo);

    dr = zr * dr * power + 1.0;
    zr *= zr0;
    
    z = zr * float3(czo*cos(zi), czo*sin(zi), sin(zo));
}

float Mandelbulb(float3 z0, inout float min_dist)
{
    float3 c = z0;
    float3 z = z0;
    float maxIterations = 8.0;
    float bailout = 2.0;

    float dr = 1.0;
    float r	 = length(z);
    if (r < min_dist) 
      min_dist = r;
    
    int n=0;
    while(n < maxIterations &&  r <= bailout)
    {
        powN(z, r, dr);
        z += c;

        r = length(z);
        if (r < min_dist) 
          min_dist = r;
        
        n++;
    }

    return 0.5 * log(r) * r / dr;
}


float DistanceEvaluation(float3 z0, inout int a_materialId)
{ 
  float min_dist = 1e38f;
  min_dist = Mandelbulb(0.75f*z0, min_dist);
  a_materialId = 2;
  return min_dist;
}




// Calculate the gradient in each dimension from the intersection point
//
float3 EstimateNormal(float3 z, float eps)
{
	float3 z1 = z + float3(eps, 0, 0);
	float3 z2 = z - float3(eps, 0, 0);
	float3 z3 = z + float3(0, eps, 0);
	float3 z4 = z - float3(0, eps, 0);
	float3 z5 = z + float3(0, 0, eps);
	float3 z6 = z - float3(0, 0, eps);

  int temp;
	float dx = DistanceEvaluation(z1,temp) - DistanceEvaluation(z2,temp);
	float dy = DistanceEvaluation(z3,temp) - DistanceEvaluation(z4,temp);
	float dz = DistanceEvaluation(z5,temp) - DistanceEvaluation(z6,temp);

	return normalize(float3(dx, dy, dz) / (2.0*eps));
}

struct Hit
{
  float3 pos;
  float3 norm;
  int materialId;
};

void Hit_init(inout Hit a_hit) {a_hit.materialId = -1;}
bool Hit_found(in Hit a_hit)   {return a_hit.materialId >= 0;}

Hit DistanceAidedRayMarch(float3 ray_pos, float3 ray_dir, float tmin, float tmax)
{
  float epsilon = 0.001f;
  
  float minStep = epsilon;
  float maxStep = 10.0f;
	
	float t = tmin;
	float dt = 1.0f;

	float dist = 1.0f; // signed distance
	
	Hit hit; Hit_init(hit);
	int matId;
	
	while(dist > epsilon && t < tmax)
	{
	  float3 p = ray_pos + t*ray_dir;
	  dist = DistanceEvaluation(p, matId);
	  dt = clamp(dist, minStep, maxStep); 
	  t += dt;
	}
	
	float3 hitPoint = ray_pos + (t-dt)*ray_dir;
	
	if(dist<=epsilon)
	{
	  hit.norm = EstimateNormal(hitPoint, epsilon);
	  hit.pos  = hitPoint;
	  hit.materialId = matId;
	}
	
	return hit;
}


float3 EvalDiffuseBRDFTerm(float3 l, float3 norm, float3 color)
{
  return color*max(dot(norm,l),0);
}

float3 EvalSpecularBRDF_Phong(float3 v, float3 l, float3 norm, float3 color, float power)
{
  float3 r = reflect((-1)*l, norm);
  return color*pow(max(dot(r,v),0), power);
}


float EstimateAmbientOcclusion(float3 pos, float3 norm)
{
    float dist;
    float sum;
    
    for (int i=0; i<5; i++)
    {   
        int matId;
        pos += norm*0.1f;
        dist = DistanceEvaluation(pos, matId);
        sum += dist;
    }
    
    float max_dist = length(norm*5.0f*0.1f);
    
    return min(0.04f*sum/(max_dist+0.1f), 1.0f);
}


float3 RayTrace(float x, float y, float w, float h)
{
  // generate initial ray
  //
  float3 ray_pos = float3(0,0,0); 
  float3 ray_dir = EyeRayDir(x,y,w,h);
 
  // transorm ray with matrix
  //
  ray_pos = (g_rayMatrix*float4(ray_pos,1)).xyz;
  ray_dir = float3x3(g_rayMatrix)*ray_dir;
 
  // intersect bounding box of the whole scene, if no intersection found return background color
  // 
  float tmin = 1e38f;
  float tmax = 0;
 
  if(!RayBoxIntersection(ray_pos, ray_dir, g_bBoxMin, g_bBoxMax, tmin, tmax))
    return g_bgColor.xyz;

	
	float  boxSize = length(g_bBoxMax - g_bBoxMin);
 
  float3 color = float3(0,0,0);
  float  k = 1.0f;
  
  for(int i=0;i<1;i++)
  {
    Hit hit = DistanceAidedRayMarch(ray_pos, ray_dir, tmin, tmax);
      
    if(!Hit_found(hit))
      break;
      
    float3 l = normalize(g_lightPos-hit.pos);
    
    float3 materialColorDiffuse  = g_matDiffuseColor;
    float3 materialColorSpecular = g_matSpecularColor;  
    
    //Hit shadowHit = DistanceAidedRayMarch(hit.pos+0.001f*hit.norm, l, 0.001f, boxSize);
    //if(!Hit_found(shadowHit))
    //{ 
      float3 diffuseTerm  = EvalDiffuseBRDFTerm(l, hit.norm, materialColorDiffuse);
      float3 specularTerm = EvalSpecularBRDF_Phong(-ray_dir, l, hit.norm, materialColorSpecular, 60);
      color += k*g_pointLightColor*(diffuseTerm + specularTerm);
    //}
    
    // calc fast and fake ambient occlusion
    //
    float ao = 0.75f*EstimateAmbientOcclusion(hit.pos, hit.norm);
    
    float3 materialColorAmbient = g_matAmbientColor + g_matDiffuseColor;
    color += k*materialColorAmbient*20.0f*ao*g_environmentLightColor;
        
    // go to next recursion level
    //    
    k *= 0.25f;
    ray_dir = reflect(ray_dir, hit.norm);
    ray_pos = hit.pos + 0.001f*hit.norm;
    tmin = 0;
    tmax = boxSize;
  }

  return color;
}


void main(void)
{	
  float w = float(g_screenWidth);
  float h = float(g_screenHeight);
  
  // get curr pixelcoordinates
  //
  float x = fragmentTexCoord.x*w; 
  float y = fragmentTexCoord.y*h;
  
  float3 color = RayTrace(x,y,w,h);
  
  //float3 color;
  //color += RayTrace(x+0.25f,y+0.25f,w,h);
  //color += RayTrace(x-0.25f,y+0.25f,w,h);
  //color += RayTrace(x-0.25f,y-0.25f,w,h);
  //color += RayTrace(x+0.25f,y-0.25f,w,h);
  //color *= 0.25f;
  
	fragColor  = float4(color,0);
}























































