#version 330

in  vec2  fragmentTexCoord;
out float outHeight;

uniform sampler2D inPositions;
uniform sampler2D inPositionsPrev;

uniform int gridSizeX;
uniform int gridSizeY;

void main(void)
{ 
  float x = fragmentTexCoord.x*float(gridSizeX);
  float y = fragmentTexCoord.y*float(gridSizeY);
  
  vec2 invSize = vec2(1.0/float(gridSizeX), 1.0/float(gridSizeY));
 
  float p = textureLod(inPositionsPrev, vec2(x,y)*invSize, 0).x;
	float m = 1.0;

	float l = textureLod(inPositions, vec2(x-1.0, y)*invSize, 0).x;
	float r = textureLod(inPositions, vec2(x+1.0, y)*invSize, 0).x;
	float u = textureLod(inPositions, vec2(x, y-1.0)*invSize, 0).x;
	float d = textureLod(inPositions, vec2(x, y+1.0)*invSize, 0).x;
	float c = textureLod(inPositions, vec2(x, y)*invSize, 0).x;	

  float sum = l + r + u + d - 4.0f*c;   
  float viscousity   = 0.01f;    
  float asquared_inv = 0.4f;
	float timestep_squared = 0.5f;	
   
	outHeight = (2.0f-viscousity)*c - (1.0f-viscousity)*p + timestep_squared*asquared_inv*sum/m; 
}

