#version 330

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;
uniform mat4 objectMatrix;

uniform int gridSizeX;
uniform int gridSizeY;

uniform sampler2D inPositions;

in vec4 vertex;
in vec2 texCoord;

out vec3 fragmentNormal;
out vec2 fragmentTexCoord;

void main(void)
{
  float x = texCoord.x*float(gridSizeX);
  float y = texCoord.y*float(gridSizeY);
  vec2 invSize = vec2(1.0/float(gridSizeX), 1.0/float(gridSizeY));
  
	float c = textureLod(inPositions, vec2(x, y)*invSize, 0).x;	
  
  float l = textureLod(inPositions, vec2(x-1.0, y)*invSize, 0).x;
	float r = textureLod(inPositions, vec2(x+1.0, y)*invSize, 0).x;
	float u = textureLod(inPositions, vec2(x, y-1.0)*invSize, 0).x;
	float d = textureLod(inPositions, vec2(x, y+1.0)*invSize, 0).x;
	
	float nx = l-r;
	float ny = u-d;
	vec3  normal = normalize(vec3(nx,ny,2.0/float(gridSizeX)));
  
  vec4 vpos = vertex + vec4(0,c,0,0); 
  
  vec4 worldPos = objectMatrix*vpos;
  vec4 viewPos  = modelViewMatrix*worldPos;
  
  fragmentNormal   = normalize(mat3(modelViewMatrix)*normal);
  fragmentTexCoord = texCoord;
  
  gl_Position = projectionMatrix*viewPos;
}
