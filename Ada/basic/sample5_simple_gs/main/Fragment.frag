#version 330

in vec3 fragmentWorldPos;
in vec3 fragmentNormal;
in vec3 fragmentFrontColor;

out vec4 fragColor;

uniform vec3 g_lightPos;
uniform vec3 g_camPos;

vec3 EvalDiffuseBRDFTerm(vec3 l, vec3 norm, vec3 color)
{
  return color*max(dot(norm,l),0);
}

vec3 EvalSpecularBRDFTerm_Phong(vec3 v, vec3 l, vec3 norm, vec3 color, float power)
{
  vec3 r = reflect((-1)*l, norm);
  return color*pow(max(dot(r,v),0), power);
}

void main(void)
{	
  vec3 v = normalize(g_camPos - fragmentWorldPos);
  vec3 n = fragmentNormal;

 
  vec3 color = vec3(0,0,0);
  
  vec3 l = normalize(g_lightPos - fragmentWorldPos);
  vec3 diffuseTerm  = EvalDiffuseBRDFTerm(l, n, vec3(0.5,0.5,0.5)*fragmentFrontColor);
  vec3 specularTerm = EvalSpecularBRDFTerm_Phong(v, l, n, vec3(0.5,0.5,0.75)*fragmentFrontColor, 80);

  color += 1.0f*(fragmentFrontColor*vec3(0.25,0.25,0.25) + diffuseTerm + specularTerm);
 
  fragColor = vec4(color,1.0f);
}



