#version 330

in vec3 fragmentViewPos;
in vec3 fragmentNormal;
in vec2 fragmentTexCoord;

out vec4 fragColor;

uniform vec3 g_lightPos[2];
uniform vec3 g_camPos;

uniform sampler2D inPosTexture;

void main(void)
{	  
  vec3 V = normalize(-g_camPos);
  
  vec3 N = fragmentNormal;
  vec3 L = normalize(vec3(-13,-2,4));
  vec3 H = normalize(N + V);

  float amb = 0.1;
  vec3 ambient  = vec3(0.5,0.5,0.75) * amb;
  vec3 diffuse  = vec3(0.5,0.5,0.75) * (1.0 - amb) * max(dot(L, N), 0.0);
  vec3 specular = 2.0*vec3(1.0, 1.0, 1.0) * pow(max(dot(H, N), 0.0), 32.0);
  
  fragColor = vec4(ambient + diffuse + specular, 1.0);
  //fragColor = vec4(fragmentNormal,1);
}
