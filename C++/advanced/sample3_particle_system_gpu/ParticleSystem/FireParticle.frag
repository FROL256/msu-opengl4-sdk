#version 400

in  vec2 fragmentTexCoord;
out vec4 fragColor;

uniform sampler2D colorTex;
uniform vec3 particleColor = vec3(1,1,1);

void main(void)
{	
  fragColor = vec4(particleColor,1)*texture(colorTex, fragmentTexCoord);
}








