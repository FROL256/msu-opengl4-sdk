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
ShaderProgram g_programCopy;

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
RenderableTexture2D* pDebugTexture = NULL;
RenderableTexture2D* pDebugTexture2 = NULL;

SimpleMesh* g_pLandMesh = NULL;
Texture2D*  g_pSnowTex = NULL;
Texture2D*  g_pSnowSpecularTex = NULL;

float4x4 g_projectionMatrix;

float3 g_camPos(0,0,20);


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

    g_program = ShaderProgram("../main/Vertex.vert", "../main/Fragment.frag");
    g_programCopy = ShaderProgram("../main/Quad.vert", "../main/Copy.frag");

    g_pFullScreenQuad  = new FullScreenQuad();
    g_pLandMesh        = new SimpleMesh(g_program.program, 2, SimpleMesh::PLANE);
    g_pSnowTex         = new Texture2D("../data/texture1.bmp");//new Texture2D("../data/snow_2_m.tga");
    g_pSnowSpecularTex = new Texture2D("../data/snow_m.tga");

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

  delete pDebugTexture;
  pDebugTexture = new RenderableTexture2D(GL_RGBA32F, width, height);

  delete pDebugTexture2;
  pDebugTexture2 = new RenderableTexture2D(GL_RGBA32F, width, height); 
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

    float3 lightPos[2];

    lightPos[0].x = 5*sin(0.10f*elaspedTimeFromStart);
    lightPos[0].y = 2.5 + 2.5*cos(0.10f*elaspedTimeFromStart);
    lightPos[0].z = 5*sin(0.10f*elaspedTimeFromStart);

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

    // land
    //

    float4x4 rotationMatrix, scaleMatrix, translateMatrix;
    float4x4 transformMatrix1, transformMatrix2;

    glusRotateRzRyRxf(rotationMatrix.L(), 90, 0.0f, 0.0f);
    glusScalef(scaleMatrix.L(), 10, 10, 10);
    glusTranslatef(translateMatrix.L(), 0,-1,0);
    glusMultMatrixf(transformMatrix1.L(), rotationMatrix.L(), scaleMatrix.L());
    glusMultMatrixf(transformMatrix2.L(), translateMatrix.L(), transformMatrix1.L());

    glUseProgram(g_program.program);

    setUniform(g_program.program, "modelViewMatrix", modelView);
    setUniform(g_program.program, "projectionMatrix", g_projectionMatrix);  // set matrix we have calculated in "reshape" funtion
    setUniform(g_program.program, "objectMatrix", transformMatrix2);

    setUniformArray(g_program.program, "g_lightPos", lightPos, 2);

    setUniform(g_program.program, "g_camPos", g_camPos);

    bindTexture(g_program.program, 1, "diffuseTexture",  g_pSnowTex->GetColorTexId());
    bindTexture(g_program.program, 2, "specularTexture", g_pSnowSpecularTex->GetColorTexId());

    g_pLandMesh->Draw();


    // \\ land
  
    if(g_debugSave)
    {
      pDebugTexture->BeginRenderingToThisTexture();
      g_pLandMesh->Draw();
      pDebugTexture->EndRenderingToThisTexture();

      SaveGLTextureToFile("debug_tex.hdr", pDebugTexture->GetColorTexId(), GL_FLOAT, 0);
      std::cerr << "screen shot made, please see (install 'The Compressonator') debug_tex.hdr" << std::endl;


      glDisable(GL_DEPTH_TEST);
      pDebugTexture2->BeginRenderingToThisTexture();
      glUseProgram(g_programCopy.program);

      bindTexture(g_programCopy.program, 1, "inTex", pDebugTexture->GetColorTexId());

      g_pFullScreenQuad->Draw();

      pDebugTexture2->EndRenderingToThisTexture();
      SaveGLTextureToFile("debug_tex2.hdr", pDebugTexture2->GetColorTexId(), GL_FLOAT, 0);
      glEnable(GL_DEPTH_TEST);

      exit(0);
    }

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
  delete g_pLandMesh; g_pLandMesh = NULL;

  delete g_pSnowTex; g_pSnowTex = NULL;
  delete g_pSnowSpecularTex; g_pSnowSpecularTex = NULL;
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

	if (!glusCreateWindow("DevIL sample (press 'i' to make screen-shot)", 640, 480, GLUS_FALSE))
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

