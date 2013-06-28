/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "../vsgl3/glHelper.h"

#include "Water.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <windows.h>

ShaderProgram g_renderRoomProg;

struct MyInput
{
  MyInput() 
  {
    cam_rot[0] = cam_rot[1] = cam_rot[2] = cam_rot[3] = 0.f;
    mx = my = 0;
    rdown = ldown = false;
    cam_dist = 20.0f;
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

float4x4 g_projectionMatrix;

float3 g_camPos(0,0,20);


SimpleMesh* g_pRoomMesh  = NULL;
Water* g_pWater = NULL;

void SetVSync(bool sync)
{
  typedef bool (APIENTRY *PFNWGLSWAPINTERVALFARPROC)(int);

  PFNWGLSWAPINTERVALFARPROC wglSwapIntervalEXT = 0;

  wglSwapIntervalEXT = (PFNWGLSWAPINTERVALFARPROC)wglGetProcAddress("wglSwapIntervalEXT");

  if( wglSwapIntervalEXT )
    wglSwapIntervalEXT(sync);
}

void CALLBACK DebugCallback(unsigned int source, unsigned int type, unsigned int id, unsigned int severity, int length, const char* message, void* userParam)
{
  std::cerr << "GL_Error  : " << message << std::endl;
}

void RequreExtentions()
{
  CHECK_GL_ERRORS;

  std::cout << "GPU Vendor: " << glGetString(GL_VENDOR) << std::endl;
  std::cout << "GPU Name  : " << glGetString(GL_RENDERER) << std::endl;
  std::cout << "GL_VER    : " << glGetString(GL_VERSION) << std::endl;
  std::cout << "GLSL_VER  : " << glGetString(GL_SHADING_LANGUAGE_VERSION) << std::endl;

  GlusHelperRequireExt h;
  h.require("GL_EXT_texture_filter_anisotropic");
  h.require("GL_ARB_debug_output");

  //std::map<std::string, bool>::const_iterator p;
  //for(p = h.supportedExtensions.begin(); p!= h.supportedExtensions.end(); ++p)
  //  std::cout << p->first << " = " << p->second << std::endl;

  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
  glDebugMessageCallbackARB(&DebugCallback, NULL);

  glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, NULL, GL_FALSE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_ERROR_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_PERFORMANCE_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_PORTABILITY_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
}

/**
* Function for initialization.
*/
GLUSboolean init(GLUSvoid)
{
  try 
  {
    RequreExtentions();
    SetVSync(1);

    // Load the source of the vertex shader. GLUS loader corrupts shaders.
    // Thats why we do that manually.
    //
    g_renderRoomProg   = ShaderProgram("../main/Room.vert", "../main/Room.frag");
    g_pFullScreenQuad  = new FullScreenQuad();
    g_pRoomMesh        = new SimpleMesh(g_renderRoomProg.program, 2,   SimpleMesh::PLANE,  1.0f);
    g_pWater           = new Water();

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

  glusPerspectivef(g_projectionMatrix.L(), 45.0f, (GLfloat) width / (GLfloat) height, 1.0f, 100.0f);  // Calculate the projection matrix
}

GLUSvoid mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y)
{
  if(!pressed)
    return;

  if (button & 1)// left button
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

    input.cam_rot[0] += 0.25f*(y1-input.my);	// change rotation
    input.cam_rot[1] += 0.25f*(x1-input.mx);

    input.mx=x;
    input.my=y;
  }

  if(button & 4)
  {
    float fov = 3.141592654f*0.5f;

    // intercect ray with plane xz
    ///
    float3 ray_pos = g_camPos;
    float3 ray_dir;
    ray_dir.x = x + 0.5f - g_width*0.5f;
    ray_dir.y = y + 0.5f - g_height*0.5f;
    ray_dir.z = -(g_width)/tanf(fov*0.5f);

    float lenDir = sqrtf(ray_dir.x*ray_dir.x + ray_dir.y*ray_dir.y + ray_dir.z*ray_dir.z);
    ray_dir.x = ray_dir.x/lenDir;
    ray_dir.y = ray_dir.y/lenDir;
    ray_dir.z = ray_dir.z/lenDir;

    float t = (0.0f - ray_pos.z)/ray_dir.z;
    float3 intersectionPos;
    intersectionPos.x = ray_pos.x + t*ray_dir.x;
    intersectionPos.y = ray_pos.y + t*ray_dir.y;
    intersectionPos.z = ray_pos.z + t*ray_dir.z;

    float x = (0.75*intersectionPos.x+5.0f)/10.0f;
    float z = (0.75*intersectionPos.y+5.0f)/10.0f;

    g_pWater->AddWave(z,x);
  }

}

GLUSvoid keyboard(GLUSboolean pressed, GLUSuint key)
{
  g_debugSave = 0;

  switch(key)
  {
  case 'w':
  case 'W':
    input.cam_dist -= 0.25f;
    break;

  case 's':
  case 'S':
    input.cam_dist += 0.25f;
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

    float3 lightPos[2];

    lightPos[0].x = 5*sin(0.10f*elaspedTimeFromStart);
    lightPos[0].y = 2.5 + 2.5*cos(0.10f*elaspedTimeFromStart);
    lightPos[0].z = 10*sin(0.10f*elaspedTimeFromStart);

    lightPos[1].x = 5*cos(0.10f*elaspedTimeFromStart);
    lightPos[1].y = 2.5 + 2.5*sin(0.10f*elaspedTimeFromStart);
    lightPos[1].z = 5*cos(0.10f*elaspedTimeFromStart);

    g_camPos.z = input.cam_dist;



    float4x4 model;
    float4x4 modelView;
    glusLoadIdentityf(model.L()); 
    glusRotateRzRyRxf(model.L(), input.cam_rot[0], input.cam_rot[1], 0.0f);
    glusLookAtf(modelView.L(), g_camPos.x, g_camPos.y, g_camPos.z, 
                               0.0f, 0.0f, 0.0f, 
                               0.0f, 1.0f, 0.0f);                           // ... and the view matrix ...

    glusMultMatrixf(modelView.L(), modelView.L(), model.L()); 	            // ... to get the final model view matrix

    // make our program current
    //
    glViewport(0, 0, g_width, g_height);
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);

    // draw room
    //
    glUseProgram(g_renderRoomProg.program);

    setUniformArray(g_renderRoomProg.program, "g_lightPos", lightPos, 2);
    setUniform(g_renderRoomProg.program, "g_camPos", g_camPos);
    setUniform(g_renderRoomProg.program, "modelViewMatrix", modelView);
    setUniform(g_renderRoomProg.program, "projectionMatrix", g_projectionMatrix);  // set matrix we have calculated in "reshape" funtion

    // set room transform
    //
    {
      float4x4 rotationMatrix, scaleMatrix, translateMatrix;
      float4x4 transformMatrix1, transformMatrix2;

      glusRotateRzRyRxf(rotationMatrix.L(), 90, 0.0f, 0.0f);
      glusScalef(scaleMatrix.L(), 10, 10, 10);
      glusTranslatef(translateMatrix.L(), 0,-4,0);
      glusMultMatrixf(transformMatrix1.L(), rotationMatrix.L(), scaleMatrix.L());
      glusMultMatrixf(transformMatrix2.L(), translateMatrix.L(), transformMatrix1.L());

      setUniform(g_renderRoomProg.program, "objectMatrix", transformMatrix2);

      setUniform(g_renderRoomProg.program, "g_diffuseColor",  float3(0.5, 0.5, 0.5));
      setUniform(g_renderRoomProg.program, "g_specularColor", float3(0.25, 0.25, 0.25));
    }
    g_pRoomMesh->Draw();




    // laplace simulations
    //
    g_pWater->SimStep();

    // draw water
    //
    glUseProgram(g_pWater->GetRenderProgram());

    setUniformArray(g_pWater->GetRenderProgram(), "g_lightPos", lightPos, 2);
    setUniform(g_pWater->GetRenderProgram(), "g_camPos", g_camPos);
    setUniform(g_pWater->GetRenderProgram(), "modelViewMatrix", modelView);
    setUniform(g_pWater->GetRenderProgram(), "projectionMatrix", g_projectionMatrix);  // set matrix we have calculated in "reshape" funtion

    // set water plane transforms
    //
    {
      float4x4 rotationMatrix, scaleMatrix, translateMatrix;
      float4x4 transformMatrix1, transformMatrix2;

      glusScalef(scaleMatrix.L(), 10, 10, 10);
      glusTranslatef(translateMatrix.L(), 0,0,0);
      glusMultMatrixf(transformMatrix1.L(), rotationMatrix.L(), scaleMatrix.L());
      glusMultMatrixf(transformMatrix2.L(), translateMatrix.L(), transformMatrix1.L());

      setUniform(g_pWater->GetRenderProgram(), "objectMatrix", transformMatrix2);
    }
    g_pWater->Draw();


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
  delete g_pFullScreenQuad; g_pFullScreenQuad = NULL;
  delete g_pWater; g_pWater = NULL;

  delete g_pRoomMesh;   g_pRoomMesh = NULL;
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

	if (!glusCreateWindow("Laplace water", 640, 480, GLUS_FALSE))
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
  
	if (!glewIsSupported("GL_VERSION_3_0"))
	{
		printf("OpenGL 3.0 not supported.");

		glusDestroyWindow();
		return -1;
	}

	glusRun();

	return 0;
}

