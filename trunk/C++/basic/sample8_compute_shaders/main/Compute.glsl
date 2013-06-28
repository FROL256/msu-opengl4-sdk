#version 430 core

layout(binding = 1)  buffer inputBuffer
{
	float Input[];
} In;

layout(binding = 2) buffer outputBuffer
{
	float Ouput[];
} Out;

layout (local_size_x = 256, local_size_y = 1, local_size_z = 1) in;

void main()
{	
	Out.Ouput[gl_GlobalInvocationID.x] = 2.0*In.Input[gl_GlobalInvocationID.x];
}
