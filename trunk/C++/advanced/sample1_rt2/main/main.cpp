/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "../vsgl3/glHelper.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <windows.h>

ShaderProgram g_program;

struct MyInput
{
  MyInput() 
  {
    cam_rot[0] = cam_rot[1] = cam_rot[2] = cam_rot[3] = 0.f;
    mx = my = 0;
    rdown = ldown = false;
    cam_dist = 5.0f;
  }

  int mx;
  int my;
  bool rdown;
  bool ldown;
  float cam_rot[4];
  float cam_pos[4];

  float cam_dist;

}input;

FullScreenQuad* g_pFullScreenQuad = NULL;
int g_width  = 0;
int g_height = 0;

int g_debugSave = 0;
RenderableTexture2D* pDebugTexture = NULL;


float3 g_boxMin(-4,-4,-4);
float3 g_boxMax(4,4,4);
float3 g_camPos(0,0,4);

float3 g_matAmbientColors[4];
float3 g_matDiffuseColors[4];
float3 g_matSpecularColors[4];

void RequreExtentions()
{
  CHECK_GL_ERRORS;

  std::cout << "GPU Vendor: " << glGetString(GL_VENDOR) << std::endl;
  std::cout << "GPU Name  : " << glGetString(GL_RENDERER) << std::endl;
  std::cout << "GL_VER    : " << glGetString(GL_VERSION) << std::endl;
  std::cout << "GLSL_VER  : " << glGetString(GL_SHADING_LANGUAGE_VERSION) << std::endl;

  //GlusHelperRequireExt h;
  //h.require("GL_EXT_texture_filter_anisotropic");
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

    g_program = ShaderProgram("../main/Vertex.vert", "../main/Fragment.frag");
    g_pFullScreenQuad = new FullScreenQuad();

    // GL init done. Now init some our internal stuff, related to materials, cameras and other
    //

    g_matAmbientColors[0]   = float3(0.2, 0.2, 0.2);
    g_matDiffuseColors[0]   = float3(0.25, 0.25, 0.25);
    g_matSpecularColors[0]  = float3(0.5, 0.5, 0.5);

    g_matAmbientColors[1]   = float3(0.0, 0.2, 0.0);
    g_matDiffuseColors[1]   = float3(0.0, 0.5, 0.0);
    g_matSpecularColors[1]  = float3(0.5, 0.5, 0.5);

    g_matAmbientColors[2]   = float3(0.25, 0.25, 0.25);
    g_matDiffuseColors[2]   = float3(0.5, 0.5, 0.5);
    g_matSpecularColors[2]  = float3(0.25, 0.25, 0.25);

    g_matAmbientColors[3]   = float3(0.1, 0.1, 0.1);
    g_matDiffuseColors[3]   = float3(0.5, 0.0, 0.5);
    g_matSpecularColors[3]  = float3(0.5, 0.0, 0.5);


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

  delete pDebugTexture;
  pDebugTexture = new RenderableTexture2D(GL_RGBA32F, width, height);
}

GLUSvoid mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y)
{
  if(!pressed)
    return;

  if (button & 1) // left button		     
  {
    input.ldown=true;		
    input.mx=x;			   
    input.my=y;
  }
  
  if (button & 4)	// right button
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

    input.cam_rot[0] += 0.25f*(y1-input.my);	//change cam rotation angle
    input.cam_rot[1] += 0.25f*(x1-input.mx);
    
    input.mx=x;
    input.my=y;
  }
}

GLUSvoid keyboard(GLUSboolean pressed, GLUSuint key)
{
  g_debugSave = 0;

  switch(key)
  {
  case 'w':
  case 'W':
    input.cam_dist -= 0.1f;
    break;

  case 's':
  case 'S':
    input.cam_dist += 0.1f;
    break;

  case 'a':
  case 'A':
   
    break;

  case 'd':
  case 'D':

  case 'i':
  case 'I':

    g_debugSave = 1;
    break;

  }

}

GLUSboolean update(GLUSfloat time)
{
  try 
  {
    static float elaspedTimeFromStart = 0;
    elaspedTimeFromStart += 10*time;

    float3 lightPos;

    lightPos.x = 10*sin(0.05f*elaspedTimeFromStart);
    lightPos.y = 10*cos(0.05f*elaspedTimeFromStart);
    lightPos.z = 10*sin(0.05f*elaspedTimeFromStart);

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

    setUniform(g_program.program, "g_lightPos", lightPos);
    setUniform(g_program.program, "g_bgColor",  float4(0,0,1,0));
    setUniform(g_program.program, "g_pointLightColor",  float3(0.75,0.75,0.75));
    setUniform(g_program.program, "g_environmentLightColor",  float3(1.25, 1.25, 1.25));

    //setUniform(g_program.program, "g_FractalPower", 8.0f + 6*sin(0.05f*elaspedTimeFromStart));

    
    // Calc ray matrix. You should notice that rayMatrix is actually inverse of worldMatrix. 
    // Because if you do some transform with an object, you need to do inverse transform with the rays. 
    // Do not forget it please!
    //
    float4x4 rayMatrix;
    float4x4 camRotMatrix, camTransMatrix;

    glusLoadIdentityf(camRotMatrix.L());
    glusLoadIdentityf(camTransMatrix.L());
    glusRotateRzRyRxf(camRotMatrix.L(), -input.cam_rot[0], -input.cam_rot[1], 0.0f);
    glusTranslatef(camTransMatrix.L(), 0, 0, input.cam_dist);
    glusMultMatrixf(rayMatrix.L(), camRotMatrix.L(), camTransMatrix.L());
    //glusMultMatrixf(rayMatrix.L(), camTransMatrix.L(), camRotMatrix.L());
    setUniform(g_program.program, "g_rayMatrix", rayMatrix);

    setUniformArray(g_program.program, "g_matAmbientColors", g_matAmbientColors, 4);
    setUniformArray(g_program.program, "g_matDiffuseColors", g_matDiffuseColors, 4);
    setUniformArray(g_program.program, "g_matSpecularColors", g_matSpecularColors, 4);


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
GLUSvoid terminate(GLUSvoid)
{
  delete g_pFullScreenQuad;

}

/**
 * Main entry point.
 */
int main(int argc, char* argv[])
{
	glusInitFunc(init);
	glusReshapeFunc(reshape);
	glusUpdateFunc(update);
	glusTerminateFunc(terminate);
  glusMouseFunc(mouse);
  glusMouseMoveFunc(mouseMove);
  glusKeyFunc(keyboard);

	glusPrepareContext(3, 0, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("C++, GLSL Fractal RT", 640, 480, GLUS_FALSE))
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

