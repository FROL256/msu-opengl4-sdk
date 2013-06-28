#version 430 core

// coherent: read and write (alternative 'readonly' and 'writeonly')
// image2D : Type of the texture (image1D, image2D, image3D, image2DRect, imageCube, imageBuffer,  image1DArray,  image2DArray, image2DMS, image2DMSArray... )
//           note that for int and uint data you have to use (iimage1D, iimage2D, ... uimage1D, umage2D)
// size1x8 : R8I, R8UI
// size1x16: R16I, R16UI
// size1x32: R32F, R32I, R32UI
// size2x32: RG32F, RG32I, RG32UI
// size4x32: RGBA32F, RGBA32I, RGBA32UI

coherent uniform layout(size4x32) image2D image;
uniform float time;

layout (local_size_x = 16, local_size_y = 16, local_size_z = 1) in;

void main()
{	
  ivec2 coords = ivec2(gl_GlobalInvocationID.x, gl_GlobalInvocationID.y);
  vec4  src    = imageLoad(image, coords);
  
  vec4 funnyColor = vec4(1,1,1,0)*abs(sin(time));
  
  if((gl_GlobalInvocationID.x/4 + gl_GlobalInvocationID.y/4)%2 == 0)
    funnyColor *= vec4(0,1,1,0); 
  else
    funnyColor *= vec4(1,1,0,0);
  
  imageStore(image, coords, src*funnyColor);
}
