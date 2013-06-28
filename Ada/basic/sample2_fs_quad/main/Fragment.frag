#version 330

in  vec2 fragmentTexCoord;
out vec4 fragColor;

void main(void)
{	
  int x = int(fragmentTexCoord.x*64.0);
  int y = int(fragmentTexCoord.y*64.0);
  
  if((x+y)%2 == 0)
    fragColor = vec4(1,1,1,1); 
  else
    fragColor = vec4(0,0,0,1); 
}























































