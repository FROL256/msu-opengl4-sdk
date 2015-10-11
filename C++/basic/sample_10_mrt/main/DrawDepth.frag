#version 330 core

in  vec2 fragmentTexCoord;
out vec4 fragColor;

uniform sampler2D inTex;

void main(void)
{
	float z = texture(inTex, fragmentTexCoord).x;
    float n = 10.0;
    float f = 30.0;
    float intensity = (2.0 * n) / (f + n - z * (f - n));
    
	fragColor = vec4( intensity , intensity , intensity , 1. );
}