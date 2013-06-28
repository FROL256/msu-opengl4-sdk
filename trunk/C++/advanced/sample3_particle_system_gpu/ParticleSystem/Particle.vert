#version 400

uniform mat4 modelViewMatrix;

in  vec4 vertex;
in  vec4 velocity;

out vec4  pointViewPos;
out float pointSize;
out float particleLive;

void main(void)
{
  pointViewPos = modelViewMatrix*vec4(vertex.xyz,1);
  pointSize    = vertex.w;
  particleLive = velocity.w;
}
