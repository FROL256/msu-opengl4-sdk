#version 330

in  vec2 fragmentTexCoord;
out vec4 fragColor;

uniform sampler2D diffuseTexture;
uniform samplerBuffer tboSampler;

void main(void)
{
  vec3 color = vec3(0,0,0);
 
  if(fragmentTexCoord.x > 0.25f && fragmentTexCoord.x < 0.75f && fragmentTexCoord.y > 0.25f && fragmentTexCoord.y < 0.75f)
  {
    int x = int(fragmentTexCoord.x*512.0);
    int y = int(fragmentTexCoord.y*512.0);

    color = texelFetch(tboSampler, y*512+x).xyz;
  }
  else
    color = 0.5*texture(diffuseTexture, fragmentTexCoord).xyz;

  fragColor = vec4(color,1.0f);
}


