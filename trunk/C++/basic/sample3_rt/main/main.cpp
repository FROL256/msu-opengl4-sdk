/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "glHelper.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <windows.h>

GLUSshaderprogram g_program;

struct MyInput
{
  MyInput() 
  {
    cam_rot[0] = cam_rot[1] = cam_rot[2] = cam_rot[3] = 0.f;
    mx = my = 0;
    rdown = ldown = false;
  }

  int mx;
  int my;
  bool rdown;
  bool ldown;
  float cam_rot[4];
  float cam_pos[4];

}input;

FullScreenQuad* g_pFullScreenQuad = NULL;
int g_width  = 0;
int g_height = 0;

float3 g_boxMin(-1,-1,-1);
float3 g_boxMax(1,1,1);
float3 g_camPos(0,0,4);

void RequreExtentions()
{
  CHECK_GL_ERRORS;

  std::cout << "GPU Vendor: " << glGetString(GL_VENDOR) << std::endl;
  std::cout << "GPU Name  : " << glGetString(GL_RENDERER) << std::endl;
  std::cout << "GL_VER    : " << glGetString(GL_VERSION) << std::endl;
  std::cout << "GLSL_VER  : " << glGetString(GL_SHADING_LANGUAGE_VERSION) << std::endl;

  GlusHelperRequireExt h;
  h.require("GL_EXT_texture_filter_anisotropic");
}

/**
* Function for initialization.
*/
GLUSboolean init(GLUSvoid)
{
  try 
  {
    RequreExtentions();

    // Load the source of the vertex shader. GLUS loader corrupts shaders.
    // Thats why we do that manually.
    //
    std::ifstream vertSourceFile("../main/Vertex.vert");
    std::ifstream fragSourceFile("../main/Fragment.frag");

    std::string fragSource;
    std::string vertSource;

    fragSourceFile.seekg(0, std::ios::end);   
    fragSource.reserve(fragSourceFile.tellg());
    fragSourceFile.seekg(0, std::ios::beg);
    fragSource.assign((std::istreambuf_iterator<char>(fragSourceFile)), std::istreambuf_iterator<char>());

    vertSourceFile.seekg(0, std::ios::end);   
    vertSource.reserve(vertSourceFile.tellg());
    vertSourceFile.seekg(0, std::ios::beg);
    vertSource.assign((std::istreambuf_iterator<char>(vertSourceFile)), std::istreambuf_iterator<char>());

    const char* tmpVertSource = vertSource.c_str();
    const char* tmpFragSource = fragSource.c_str();

    if(!glusBuildProgram(&g_program, (const GLUSchar**)&tmpVertSource, 0, 0, 0, (const GLUSchar**)&tmpFragSource))
      throw std::runtime_error("shader compilation failed!");

    g_pFullScreenQuad = new FullScreenQuad();

    // GL init done. Now init some our internal stuff, related to materials, cameras and other
    //

    return GLUS_TRUE;
  }
  catch(std::runtime_error e)
  {
    std::cerr << e.what() << std::endl;
    exit(-1);
  }
  catch(...)
  {
    std::cerr << "Unexpected Exception (init)!" << std::endl;
    exit(-1);
  }
}

GLUSvoid reshape(GLUSuint width, GLUSuint height)
{
	glViewport(0, 0, width, height);
  g_width  = width;
  g_height = height;
}

GLUSvoid mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y)
{
  if(!pressed)
    return;

  if (button & 1)		    //left button pressed
  {
    input.ldown=true;		//remember flag
    input.mx=x;			    //remember coords
    input.my=y;
  }
  
  if (button & 4)	      //right button pressed
  {
    input.rdown=true;
    input.mx=x;
    input.my=y;
  }
}

GLUSvoid mouseMove(GLUSuint button, GLUSint x, GLUSint y)
{
  if(button & 1)		// left button
  {
    int x1 = x;
    int y1 = y;

    input.cam_rot[0] += 0.25f*(y1-input.my);	//change rotation angle
    input.cam_rot[1] += 0.25f*(x1-input.mx);
    
    input.mx=x;
    input.my=y;
  }
}

GLUSvoid keyboard(GLUSboolean pressed, GLUSuint key)
{

  switch(key)
  {
  case 'w':
  case 'W':
    
    break;

  case 's':
  case 'S':
    
    break;

  case 'a':
  case 'A':
   
    break;

  case 'd':
  case 'D':

    break;

  }

}

GLUSboolean update(GLUSfloat time)
{
  try 
  {
    static float elaspedTimeFromStart = 0;
    elaspedTimeFromStart += 10*time;

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClearDepth(1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    // make our program current
    //
    glUseProgram(g_program.program);  CHECK_GL_ERRORS;  

    // Set shader variables.
    // Note, that you can only assign shader variables (bind or set uiforms) 
    // if appropriate program was bind as current by glUseProgram.
    //
    setUniform(g_program.program, "g_screenWidth",  g_width);
    setUniform(g_program.program, "g_screenHeight", g_height);

    setUniform(g_program.program, "g_bBoxMin",  g_boxMin);
    setUniform(g_program.program, "g_bBoxMax",  g_boxMax);
 
    setUniform(g_program.program, "g_bgColor",  float4(0,0,1,0));
   
    
    // Calc ray matrix. You should notice that rayMatrix is actually inverse of worldMatrix. 
    // Because if you do some transform with an object, you need to do inverse transform with the rays. 
    // Do not forget it please!
    //
    float4x4 rayMatrix;
    float4x4 camRotMatrix, camTransMatrix;

    glusLoadIdentityf(camRotMatrix.L());
    glusLoadIdentityf(camTransMatrix.L());
    glusRotateRzRyRxf(camRotMatrix.L(), -input.cam_rot[0], -input.cam_rot[1], 0.0f);
    glusTranslatef(camTransMatrix.L(), g_camPos.x, g_camPos.y, g_camPos.z);
    glusMultMatrixf(rayMatrix.L(), camRotMatrix.L(), camTransMatrix.L());
    setUniform(g_program.program, "g_rayMatrix", rayMatrix);


    // Now finally draw something
    // disable depth and cull tests, because we just want to draw full screen quad
    //
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    glViewport(0, 0, g_width, g_height);
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    
    g_pFullScreenQuad->Draw();
  
    return GLUS_TRUE;
  }
  catch(std::runtime_error e)
  {
    std::cerr << e.what() << std::endl;
    exit(-1);
  }
  catch(...)
  {
    std::cerr << "Unexpected Exception(render)!" << std::endl;
    exit(-1);
  }
}

/**
 * Function to clean up things.
 */
GLUSvoid shutdown(GLUSvoid)
{
  delete g_pFullScreenQuad;

	// Delete shader program etc..
	glusDestroyProgram(&g_program);
}

/**
 * Main entry point.
 */
int main(int argc, char* argv[])
{
	glusInitFunc(init);
	glusReshapeFunc(reshape);
	glusUpdateFunc(update);
	glusTerminateFunc(shutdown);
  glusMouseFunc(mouse);
  glusMouseMoveFunc(mouseMove);
  glusKeyFunc(keyboard);

	glusPrepareContext(3, 0, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("C++, GLSL Ray Marching sample", 640, 480, GLUS_FALSE))
	{
		printf("Could not create window!");
		return -1;
	}

	// Init GLEW
	glewExperimental = GL_TRUE;
  GLenum err=glewInit();
  if(err!=GLEW_OK)
  {
    sprintf("glewInitError", "Error: %s\n", glewGetErrorString(err));
    return -1;
  }
  glGetError(); // flush error state variable, caused by glew errors
  

	// Only continue, if OpenGL 3.3 is supported.
	if (!glewIsSupported("GL_VERSION_3_0"))
	{
		printf("OpenGL 3.0 not supported.");

		glusDestroyWindow();
		return -1;
	}

	glusRun();

	return 0;
}

