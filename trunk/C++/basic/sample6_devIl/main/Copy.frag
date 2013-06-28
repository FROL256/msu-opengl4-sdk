#version 330

in  vec2 fragmentTexCoord;
out vec4 fragColor;

uniform sampler2D inTex;

void main(void)
{
	fragColor = textureLod(inTex, fragmentTexCoord, 0);
}

