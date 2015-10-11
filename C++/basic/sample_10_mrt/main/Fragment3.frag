#version 330 core

#extension all: warn

//#include "math.h"


// just don't like these OGL's vec"n", cause in (DX,CUDA,Cg) they are float"n" 
//
#define float2 vec2
#define float3 vec3
#define float4 vec4
#define float4x4 mat4
#define float3x3 mat3
#define complex float2


//-------------------------------------------------------------------------------------------------------
struct SComplex 
{
  float m_re, m_im;
};

SComplex SumComplex(SComplex in_first, SComplex in_second)
{
  SComplex r;
  r.m_re = in_first.m_re + in_second.m_re;
  r.m_im = in_first.m_im + in_second.m_im;
  return r;
}

SComplex MultComplex(SComplex in_first, SComplex in_second)
{
    SComplex r;
	r.m_re = in_first.m_re * in_second.m_re - in_first.m_im * in_second.m_im;
	r.m_im = in_first.m_re * in_second.m_im + in_first.m_im * in_second.m_re;
	return r;
}

SComplex exp_i(float in_phi)
{
	SComplex r;
	r.m_re = cos(in_phi);
	r.m_im = sin(in_phi);
	return r;
};


SComplex DivideComplex(SComplex in_first, SComplex in_second)
{
	SComplex r;
	float a = in_first.m_re, b = in_first.m_im;
	float c = in_second.m_re, d = in_second.m_im;
	r.m_re = (a * c + b * d) / (c * c + d * d);
	r.m_im = (b * c - a * d) / (c * c + d * d);
	return r;
}

SComplex FloatToComplex(float in_f)
{
	SComplex r;
	r.m_re = in_f;
	r.m_im = 0;
	return r;
}

struct SSellmeierCoefficients
{
  float m_A1, m_B1, m_B2, m_B3, m_B4, m_C1, m_C2, m_C3, m_C4;
  int m_coefficientsNumber;
};

struct SMedium
{
  float m_ior;
  SSellmeierCoefficients m_sellmeier;
  // =true if m_ior, = false if we must calculate ior with the help of m_sellmeier 
  bool m_simpleIor;
};

struct SFilmSystem
{
  
  float m_thickness[1];
  SMedium m_mediums[1];
  SMedium m_lowerMedium;
  SMedium m_upperMedium;
  float m_k;
  
};


//----------------------------------------------------------------------------------------------------------
// creates all nessesary objects
SFilmSystem Init()
{
  //create S
  //BK7
  SSellmeierCoefficients lS;
  SSellmeierCoefficients filmS1;
 
  lS.m_B1 = 1.03961212;
  lS.m_B2 = 0.231792344;
  lS.m_B3 = 1.01046945;
  lS.m_C1 = 6.00069867e-3;
  lS.m_C2 = 2.00179144e-2;
  lS.m_C3 = 1.03560653e+2;

  lS.m_coefficientsNumber = 6;
      
  //fused silica   
  filmS1.m_B1 = 0.696166300;
  filmS1.m_B2 = 0.407942600;
  filmS1.m_B3 = 0.897479400;
  filmS1.m_C1 = 4.67914826e-3;
  filmS1.m_C2 = 1.35120631e-2;
  filmS1.m_C3 = 97.9340025;

  filmS1.m_coefficientsNumber = 6;

  //create mediums
  SMedium uM, lM, filmM1;
  lM.m_simpleIor = false;
  lM.m_sellmeier = lS;
  uM.m_simpleIor = true;
  uM.m_ior = 1.000;
  filmM1.m_simpleIor = false;
  filmM1.m_sellmeier = filmS1;


  //create film
  SFilmSystem fs; 
  fs.m_thickness[0] = 100.0f;
  fs.m_mediums[0] = filmM1;
  fs.m_lowerMedium = lM;
  fs.m_upperMedium = uM;
  fs.m_k = 1;
  return fs;

}


//in_lambda in degrees
float IorS(SSellmeierCoefficients in_s, int in_lambda)
{
    float n_2;
	float lambda_2 = in_lambda * 1e-3;
	lambda_2 *= lambda_2;
    int coefficientsNumber = in_s.m_coefficientsNumber;
    float A1, B1, B2, B3, B4, C1, C2, C3, C4;

	A1 = in_s.m_A1;
	B1 = in_s.m_B1;
	B2 = in_s.m_B2;
	B3 = in_s.m_B3;
	B4 = in_s.m_B4;
	C1 = in_s.m_C1;
	C2 = in_s.m_C2;
	C3 = in_s.m_C3;
	C4 = in_s.m_C4;

	if(coefficientsNumber == 5)
    {
      n_2 = A1 + (B1 * lambda_2 / (lambda_2 - C1))
			        + (B2 * lambda_2 / (lambda_2 - C2));
    }
    
	if(coefficientsNumber == 6)
    {
      n_2 = 1 + lambda_2 * B1 / (lambda_2 - C1) +
				  lambda_2 * B2 / (lambda_2 - C2) + 
				  lambda_2 * B3 / (lambda_2 - C3);

    }

    if(coefficientsNumber == 8)
    {
      n_2 = 1 + lambda_2 * B1 / (lambda_2 - C1) +
				  lambda_2 * B2 / (lambda_2 - C2) + 
				  lambda_2 * B3 / (lambda_2 - C3) +
          lambda_2 * B4 / (lambda_2 - C4);
    }

	if(n_2 < 0)
	{
		//?
	}
	float n = sqrt(n_2);
	return n;
}


//in_lambda in degrees
float Ior(SMedium in_s, int in_lambda)
{
  if(in_s.m_simpleIor)
  {
	  return in_s.m_ior;

  }
  else
  {
	  //in_lambda in degrees
	  return IorS(in_s.m_sellmeier, in_lambda);
  }

}




//----------------------------------------------------------------------------------------------------------


in float2 fragmentTexCoord;

layout(location = 0) out vec4 fragColor;

uniform int g_screenWidth;
uniform int g_screenHeight;

uniform float3 g_bBoxMin;
uniform float3 g_bBoxMax;

uniform float3 g_lightPos;

uniform float4x4 g_rayMatrix;
uniform float4 g_bgColor;

uniform float3 g_center;
uniform float g_radius;

out float3 outray_dir;

float Unwholepart(int x)
{
	
	float a1 = 10.0;
	//a1 = (float) x;
    

	//if (a1 > (float) x )
      //  a1 = (float) x;

	float rez = 0;      //(x - intx);
	return rez;
	
}


float3 EyeRayDir(float x, float y, float w, float h, float d)
{
	/*float fov = 3.141592654f/(2.0f); 
    float3 ray_dir;
  
	ray_dir.x = x+0.5f - (w/2.0f);
	ray_dir.y = y+0.5f - (h/2.0f);
	ray_dir.z = -(w)/tan(fov/2.0f);
	*/

	float3 ray_dir;
	ray_dir.x = (w / 1.0f) * x;
	ray_dir.y = (h / 1.0f) * y;
	ray_dir.z = -d;
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


float sp(float3 a, float3 b)
{
  return (a.x * b.x + a.y * b.y + a.z * b.z);
}

float abs3(float3 a)
{
  return sqrt(a.x * a.x + a.y * a.y + a.z * a.z);
}

float3 norm(float3 a)
{
  float m = abs3(a);
  if (m < 0.0001)
  {
    return a;
  }
  return float3(a.x / m, a.y / m, a.z / m);
}

float Reflectance(float3 ray_pos, float tmin, float3 ray_dir, float3 source_pos)
{
  // calculate poi
  float poix, poiy, poiz;
  poix = ray_pos.x + tmin * ray_dir.x;
  poiy = ray_pos.y + tmin * ray_dir.y;
  poiz = ray_pos.z + tmin * ray_dir.z;
  
  float3 poi = float3(poix, poiy, poiz);
  float res;
  float3 wo = float3(ray_pos.x - poix, ray_pos.y - poiy, ray_pos.z - poiz);
  float3 wi = float3(source_pos.x - poix, source_pos.y - poiy, source_pos.z - poiz);
  res = sp(wi, wo) / (abs3(wi) * abs3(wo));
  
  return res;
}


bool RayRectangleIntersection(float3 A, float3 m, 
							  float3 v1, float3 v2, float3 v3, float3 v4,
							  float t,
							  inout float3 Pr)
{
  return true;
}


bool RaySphereIntersection(float3 A, float3 m, float3 C, float r, inout float tmin, inout float tmax, 
						   inout float3 Pt1, inout float3 Pt2)
{
	//if (ray_dir.x > 0 && ray_dir.y > 0) return true;
	//else return false;
	/// A = u * u, B = 2(u * (p0 Ц c)) и C = (p0 Ц c) „ (p0 Ц c) Ц r2

	//int CrossLS(CVector3D &Pt1, CVector3D &Pt2, CVector3D &A, CVector3D &m, CVector3D &C, float r)


    float3 Delta = A - C;

	if(length(Delta)<0.1)
  {
    return false;
  }

	float a,b,c;
	a= length(m);//m*m;
	b=dot(Delta, m);//Delta*m;
	c=float(dot(Delta, Delta) - r * r);//Delta*Delta-r*r;

	float D=b*b-a*c;

	float t1,t2;
	
	int NPts=0;

	/*if(D==0)
		return false;*/

	if(D>=0.0000)
	{
		D=sqrt(D);

		t1=(-b+D)/a;
		t2=(-b-D)/a;

		if(t1>t2)	
		{
			float Tmp=t1;
			t1=t2;
			t2=Tmp;
		}
		if(t2 < 0.0) 
		{
			NPts = 0;
		}
		else if(t2>=0.0 && t1 < 0.0) 
		{
			NPts = 1;
			Pt2=A+m*t2;
		
		}
		else
		{
			NPts = 2;
			Pt2=A+m*t2;
		    Pt1=A+m*t1;
		}
	
	}

 
	tmin = t1;
	tmax = t2;

	return (NPts >= 1);



	/*
  float a, b, c;
  a = sp(ray_dir, ray_dir);
  b = 2 * (sp(ray_pos, ray_dir) - sp(ray_dir, center));
  c = sp(ray_pos, ray_pos) + sp(center, center) - 2 * sp(ray_pos, center) - radius * radius;
  float D = b * b - 4 * a * c;
  if(D < -1)
  {
	  tmin = tmax = -1;
	  return false;
  }
  else
  {
	tmin = (-b - sqrt(D)) / (2 * a);
    tmax = (-b + sqrt(D)) / (2 * a);
    if(tmax <= 0) return false;
  	return true;
  }
  return (tmin <= tmax) && (tmax > 0.f);*/
}

float3 RayMarchConstantFog(float tmin, float tmax, inout float alpha)
{
  float3 color = float3(0,0,0);
  float dt = 0.05f;
  float t  = tmin;
	
  alpha  = 1.0f;

	
  while(t < tmax && alpha > 0.01f)
  {
    float a = 0.05f;
    color += a*alpha*float3(1.0f,1.0f,0.0f);
    alpha *= (1.0f-a);
    t += dt;
  }
	
  return color;
}

float3 Normal(float3 P)
{
	return norm(P - g_center);
}

float3 Phong(float3 P, float3 ray_dir)
{
	float3 normal = Normal(P);
	float3 wo = float3(-ray_dir.x, -ray_dir.y, -ray_dir.z);
	float sp = dot(normal, norm(wo));

	float pi = 3.141596;
    float angle = acos(sp); 
	if(angle > pi / 2) sp = 0;
	
	return float3(sp, sp, sp);

}



/////////////////////////////////////////////////////////////////////////////////////////////

void FresnelCoefficients(float in_upper_n, float in_lower_n,
	  	                 float in_theta,
		  	             inout SComplex out_r, inout SComplex out_t, inout bool out_fullInternalReflection)

 {
	    
	    float r_parallel, r_perp,
	  	       t_parallel, t_perp;
		float n_i, n_t;
		float theta_i, theta_t;
		float expressi;
		n_i = in_upper_n;
		n_t = in_lower_n;
		theta_i = in_theta;
		///sin(theta_t)/sin(theta_i) = n_i/n_t ==> theta_t = arcsin(sin(theta_i) * n_t / n_i)
	    expressi = sin(theta_i) * n_i / n_t;
		// ”читываем полное внутреннее отражение
		if(expressi < 1)
			
		{
          out_fullInternalReflection = false;

		  theta_t = asin(expressi);
		  r_parallel = (n_t * cos(theta_i) - n_i * cos(theta_t)) 
				          / (n_t * cos(theta_i) + n_i * cos(theta_t));
		  r_perp = (n_i * cos(theta_i) - n_t * cos(theta_t)) 
				          / (n_i * cos(theta_i) + n_t * cos(theta_t));           
		  t_parallel = (2.0 * n_i * cos(theta_i)) 
			            / (n_t * cos(theta_i) + n_i * cos(theta_t));
	  	  t_perp = (2.0 * n_i * cos(theta_i))
			            / (n_i * cos(theta_i) + n_t * cos(theta_t));
	   	  out_r.m_re = r_parallel;
		  out_r.m_im = r_perp;
		  out_t.m_re = t_parallel;
		  out_t.m_im = t_perp;
		}
		else
		{
		  // Full internal reflection
          out_fullInternalReflection = true;
		  out_r.m_re = float(1.0); //sqrt(2.0) / 2;
		  out_r.m_im = float(1.0); //sqrt(2.0) / 2;
		  out_t.m_re = float(0.0);
		  out_t.m_im = float(0.0);
		}
		
}


/////////////////////////////////////////////////////////////////////////////////////////////

void KrAndKt(float in_angle, int in_lambda, inout float out_gamma, inout float out_tau, SFilmSystem in_fs)
{

  float pi = 3.141596;
  bool fullInternalReflection;
  const int layerNumber = 1;
  const int layerNumber1 = layerNumber + 1; 
  const int layerNumber2 = layerNumber + 2; 

  float n[layerNumber2];
  float nm[3];

  if(in_angle > pi / 2.0) 
  {
    out_gamma = 0.1;
	out_tau = 0.0;
    return;
  }
    
  if(in_fs.m_thickness.length() == 0)
  {
  	//float for_upper_n = Ior(g_upperMedium, in_lambda);
	//float for_lower_n = Ior(g_lowerMediumin_lambda);
	/*complex gamma_complex, tau_complex;
	FresnelCoefficients(for_upper_n, for_lower_n,
  	                  in_angle,
	  									gamma_complex, tau_complex, fullInternalReflection);
	/// here FIR can't influence on smth
	*out_gamma = m_k * abs(gamma_complex) * abs(gamma_complex);
	*out_tau = m_k * abs(tau_complex) * abs(tau_complex);
	*/
	
  }
  else
  {
	  
	
	
    /// For many-layer film
    int layerNumber = in_fs.m_mediums.length();
    

    ///Ior-s - from up to the down
    
	float n[3];
	n[0] = Ior(in_fs.m_upperMedium, in_lambda);
	Ior(in_fs.m_upperMedium, in_lambda);

	if(in_fs.m_thickness[0] == 100.0f)
	{
		out_gamma = 0.4f;
		return;
	}

	out_gamma = 0.8f;
    return;

    for(int i = 0; i < layerNumber; i++)
      n[i + 1] = Ior(in_fs.m_mediums[layerNumber - (i + 1)], in_lambda);
    n[layerNumber1] = Ior(in_fs.m_lowerMedium, in_lambda);
    

	

	/*if(n[2] > 0.5)
	{
		out_gamma = 1.0;
		return;
	}
	*/
	
	
	///All angles in radians
    float theta[layerNumber2]; //0, theta1, theta2, theta3;
    
	theta[0] = in_angle;
    float asin_arg;
	
	
	for(int i = 0; i < layerNumber + 1; i++)
	{
      asin_arg = n[i] * sin(theta[i]) / n[i + 1];
      if(asin_arg < 1)
	  	  theta[i + 1] = asin(asin_arg);
	  else
	  	  theta[i + 1] = 0;
	}
    

	
    ///Thickness of thilms from up to the down
    float d[layerNumber1]; //dliya udobstva chtobi nachinat c 1-chki
    for(int i = 0; i < layerNumber; i++)
      d[i + 1] = in_fs.m_thickness[layerNumber - (i + 1)];

	
   
	out_gamma = d[1];
	return;
	
    ///Calculating all phase differenses - from up to the down
    float deltaPhi[layerNumber1]; //dliya udobstva chtobi nachinat c 1-chki
    
    for(int i = 0; i < layerNumber; i++)
      deltaPhi[i + 1] = (2 * pi / in_lambda) *  2 * n[i + 1] * d[i + 1] * cos(theta[i + 1]);
   
    ///Lambda, thickness - from mn to metrs    

	
	
	SComplex r[layerNumber2], t[layerNumber2];
    //dliya udobstva chtobi nachinat c 1-chki
    //dliya udobstva chtobi nachinat c 1-chki
    
	

	/// The amount of layers that were gone REAL by the ray - without FIR
	int layerNumberGoneThrough = 0;

	
    for(int i = 0; i < layerNumber + 1; i++)
	{
      FresnelCoefficients(n[i], n[i + 1], theta[i], r[i + 1], t[i + 1], fullInternalReflection);
	  
	  if(fullInternalReflection)
	  {
        break;
	  }
	  else
	  {
	    layerNumberGoneThrough++;
	  }
	  
	}

	
	if(!fullInternalReflection) layerNumberGoneThrough--;


	
    SComplex gamma[layerNumber1], tau[layerNumber1];
    //dliya udobstva chtobi nachinat c 1-chki
    //dliya udobstva chtobi nachinat c 1-chki
    
    int N = layerNumberGoneThrough;
    
    gamma[0] = r[N + 1]; 
    tau[0] = t[N + 1];
    
    for(int i = 0; i < layerNumberGoneThrough; i++)
    {
      gamma[i + 1] = DivideComplex(SumComplex(r[layerNumberGoneThrough - i] , MultComplex(gamma[i], exp_i(deltaPhi[layerNumberGoneThrough - i]))),
                    SumComplex(FloatToComplex(1.0), MultComplex(r[layerNumberGoneThrough - i], MultComplex(gamma[i], exp_i(deltaPhi[layerNumberGoneThrough - i])))));
      /*gamma[i + 1] = (r[layerNumberGoneThrough - i] + gamma[i] * exp_i(deltaPhi[layerNumberGoneThrough - i])) /
                     (1 + r[layerNumberGoneThrough - i] * gamma[i] * exp_i(deltaPhi[layerNumberGoneThrough - i]));
      
	  */
	  tau[i+1] = DivideComplex(MultComplex(t[layerNumberGoneThrough - i], MultComplex(tau[i], exp_i(deltaPhi[layerNumberGoneThrough - i]))),
		         SumComplex(FloatToComplex(1.0), MultComplex(r[layerNumberGoneThrough - i], MultComplex(gamma[i], exp_i(deltaPhi[layerNumberGoneThrough - i])))));
	  
	  tau[i+1] = DivideComplex(tau[i+1], FloatToComplex(2.0));

	  /*tau[i + 1] = (t[layerNumberGoneThrough - i] * tau[i] * exp_i(deltaPhi[layerNumberGoneThrough - i] / 2) ) / 
                   (1 + r[layerNumberGoneThrough - i] * gamma[i] * exp_i(deltaPhi[layerNumberGoneThrough - i]));*/
    }

	
    out_gamma = in_fs.m_k * 0.5 * (gamma[layerNumberGoneThrough].m_im * gamma[layerNumberGoneThrough].m_im 
      + gamma[layerNumberGoneThrough].m_re * gamma[layerNumberGoneThrough].m_re);


	//*out_tau = m_k * 0.5 * (1 - (*out_gamma / (m_k * 0.5)));
    
	out_tau = in_fs.m_k * 0.5 * (tau[layerNumberGoneThrough].m_im * tau[layerNumberGoneThrough].m_im 
      + tau[layerNumberGoneThrough].m_re * tau[layerNumberGoneThrough].m_re);
	
    
    

    
  }
 
}




/////////////////////////////////////////////////////////////////////////////////////////////
void CalculateReflectanceAndTransmittanceForFilmSystem(int in_lambda,
                                                       float3 in_wi, float3 in_normal, float3 in_wo,
                                  ///ѕока толщина у всех слоев будет одинакова
                                  inout float out_gamma,
								  inout float out_tau)

{
  
  float l1 = length(in_wi), l2 = length(in_normal);
  float angle = acos(dot(norm(in_wi), norm(in_normal)));
  float pi = 3.141596;
  if(angle > pi / 2.0) 
  {
     
    angle = acos(dot(norm(in_wi), norm(-in_normal)));

  }
  //if(m_smoothness)
  //{
    //KrAndKt(angle, in_lambda, out_gamma, out_tau);
	
  //}
  //else
  //{
    //Kd(in_wi, in_normal, in_wo, in_lambda, out_gamma, out_tau);
  //}
}

/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////////

float3 Phong2(float3 P, float3 ray_dir, SFilmSystem in_fs)
{
	float3 normal = Normal(P);
	float3 wo = float3(-ray_dir.x, -ray_dir.y, -ray_dir.z);
	float sp = dot(normal, norm(wo));

	float pi = 3.141596;
    float angle = acos(sp); 
	if(angle > pi / 2) 	return float3(0.0, 0.0, 0.0);


	float gamma, tau;
    KrAndKt(angle, 380, gamma, tau, in_fs);
	
	return float3(gamma, gamma, gamma);

}



void main(void)
{	
  //int * t = (int *)malloc(sizeof(int));
  float w = float(g_screenWidth);
  float h = float(g_screenHeight);
  
  // get curr pixelcoordinates
  //
  float x = fragmentTexCoord.x * 1; 
  float y = fragmentTexCoord.y * 1;

/*fragColor = float4(fragmentTexCoord.x,fragmentTexCoord.y,0, 1);
return;*/
  
  // generate initial ray
  //
  float3 ray_pos = float3(0,0,0); 
  float3 ray_dir = EyeRayDir(x, y, w, h, 550.0);

  
  outray_dir = ray_dir;
 
  
  
  // transform ray with matrix
  //
  //ray_pos = (g_rayMatrix*float4(ray_pos,1)).xyz;
  //ray_dir = float3x3(g_rayMatrix)*ray_dir;
 
  // intersect bounding box of the whole scene, if no intersection found return background color
  // 
  float tmin = 1e38f;
  float tmax = 0;


  const int uu = 7;
  float yyy[uu];

  /*
  if(length(g_center)<0.001)
  {
	fragColor = g_bgColor;
    return;
  }*/
  
  /*if(length(g_center - float3(0.0, 0.0, -10.0)) < 0.001 && g_radius == 2.0)
  {
     fragColor = float4(1.0, 0.0, 0.0, 1.0);
	 return;
  }*/
  

  //if(!RayBoxIntersection(ray_pos, ray_dir, g_bBoxMin, g_bBoxMax, tmin, tmax))
  float3 P1, P2;
  if(!RaySphereIntersection(ray_pos, ray_dir, g_center, g_radius, tmin, tmax, P1, P2))
  {
    fragColor = g_bgColor;
    return;
  }


  /*
  float3 sourcepos = float3(0, 0, 0);
  float reflected;
  if(tmin <= 0)
  {
    reflected = Reflectance(ray_pos, tmax, ray_dir, sourcepos);
  }
  else
  {
    reflected = Reflectance(ray_pos, tmin, ray_dir, sourcepos);
  
  }*/ 


  //KrAndKt();


  SFilmSystem fs;
  fs = Init();
  
	
	float alpha = 1.0f;
	//float3 color = RayMarchConstantFog(tmin, tmax, alpha);
	//float3 color = float3(0.0, 1.0, 0.0); 
	float3 color = Phong2(P1, ray_dir, fs);
	ray_dir = norm(ray_dir);

	fragColor = float4(color, 1);
    //fragColor = float4(fs.m_thickness[0], 0, 0 , 1);
	
	//fragColor = float4(ray_dir.x + 0.5, ray_dir.y + 0.5, ray_dir.z + 0.5, 1);//float4(color,0)*(1.0f-alpha) + g_bgColor*alpha;
	//return float4(ray_dir, 1);
}























































