/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"

#include "../vsgl3/glHelper.h"

#include "../ParticleSystem/IParticleSystem.h"
#include "../ParticleSystem/GPUParticleSystem.h"
#include "../ParticleSystem/SparksParticleSystem.h"
#include "../ParticleSystem/FireParticleSystem.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <windows.h>

ShaderProgram g_landProgram;
ShaderProgram g_postProcessProgram;

ShaderProgram g_extractBrightPixelsProgram;
ShaderProgram g_blurProgram;
ShaderProgram g_bloomProgram;

struct MyInput
{
  MyInput() 
  {
    cam_rot[0] = cam_rot[1] = cam_rot[2] = cam_rot[3] = 0.f;
    mx = my = 0;
    rdown = ldown = false;
    posprocessEffectId = 2;
  }

  int mx;
  int my;
  bool rdown;
  bool ldown;
  float cam_rot[4];
  float cam_pos[4];
  int posprocessEffectId;

}input;

FullScreenQuad* g_pFullScreenQuad = NULL;
SimpleMesh* g_landMesh = NULL;

RenderableTexture2D* g_pFullScreenTexture = NULL;
RenderableTexture2D* g_pPinPongTex1 = NULL;
RenderableTexture2D* g_pPinPongTex2 = NULL;

Texture2D* g_pSnowTex = NULL;
Texture2D* g_pSnowSpecularTex = NULL;

IParticleSystem* pParticleSystem = NULL;

int g_width  = 0;
int g_height = 0;
float3 g_camPos(0,0,20);

float4x4 g_projectionMatrix;

float3 g_matAmbientColors[4];
float3 g_matDiffuseColors[4];
float3 g_matSpecularColors[4];

//HSTREAM g_sound = 0;
std::string g_fileName;

void RequreExtentions()
{
  CHECK_GL_ERRORS;

  std::cout << "GPU Vendor: " << glGetString(GL_VENDOR) << std::endl;
  std::cout << "GPU Name  : " << glGetString(GL_RENDERER) << std::endl;
  std::cout << "GL_VER    : " << glGetString(GL_VERSION) << std::endl;
  std::cout << "GLSL_VER  : " << glGetString(GL_SHADING_LANGUAGE_VERSION) << std::endl;

  GlusHelperRequireExt h;

  h.require("GL_EXT_texture_filter_anisotropic");
  h.require("GL_ARB_transform_feedback3");
  h.require("GL_ARB_geometry_shader4");
}


/**
* Function for initialization.
*/
GLUSboolean init(GLUSvoid)
{
  try 
  {
    RequreExtentions();

    g_landProgram = ShaderProgram("../main/Land.vert", "../main/Land.frag");

    g_postProcessProgram = ShaderProgram("../main/Quad.vert", "../main/Grayscale.frag");
    g_extractBrightPixelsProgram = ShaderProgram("../main/Quad.vert", "../main/ExtractBrightPixels.frag");
    g_blurProgram  = ShaderProgram("../main/Quad.vert", "../main/Blur.frag");
    g_bloomProgram = ShaderProgram("../main/Quad.vert", "../main/FinalBloomPass.frag");

    g_pFullScreenQuad = new FullScreenQuad();
    g_landMesh        = new SimpleMesh(g_landProgram.program, 2, SimpleMesh::PLANE);

    g_pSnowTex = new Texture2D("../data/texture1.bmp");//new Texture2D("../data/snow_2_m.tga");
    g_pSnowSpecularTex = new Texture2D("../data/snow_m.tga");

    // GL init done. Now init some our internal stuff, related to materials, cameras and other
    //
    g_matAmbientColors[0]   = float3(0.2f, 0.2f, 0.2f);
    g_matDiffuseColors[0]   = float3(0.5f, 0.5f, 0.5f);
    g_matSpecularColors[0]  = float3(0.75f, 0.75f, 0.75f);

    g_matAmbientColors[1]   = float3(0.0f, 0.2f, 0.0f);
    g_matDiffuseColors[1]   = float3(0.0f, 0.5f, 0.0f);
    g_matSpecularColors[1]  = float3(0.5f, 0.5f, 0.5f);

    g_matAmbientColors[2]   = float3(0.25f, 0.25f, 0.25f);
    g_matDiffuseColors[2]   = float3(0.5f, 0.5f, 0.5f);
    g_matSpecularColors[2]  = float3(0.25f, 0.25f, 0.25f);

    g_matAmbientColors[3]   = float3(0.1f, 0.1f, 0.1f);
    g_matDiffuseColors[3]   = float3(0.5f, 0.0f, 0.5f);
    g_matSpecularColors[3]  = float3(0.5f, 0.0f, 0.5f);


    //pParticleSystem = new GPUparticleSystem(2048);
    pParticleSystem = new SparksParticleSystem(4096);//(16384); 
    //pParticleSystem = new FireParticleSystem(4096); 

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

/**
* Function to clean up things.
*/
GLUSvoid shutdown(GLUSvoid)
{
  delete g_pFullScreenQuad; g_pFullScreenQuad = NULL;
  delete g_landMesh; g_landMesh = NULL;

  delete g_pSnowTex; g_pSnowTex = NULL;
  delete g_pSnowSpecularTex; g_pSnowSpecularTex = NULL;

  delete pParticleSystem; pParticleSystem = NULL;
}


GLUSvoid reshape(GLUSuint width, GLUSuint height)
{
  try 
  {
    glViewport(0, 0, width, height);
    g_width  = width;
    g_height = height;

    glusPerspectivef(g_projectionMatrix.L(), 40.0f, (GLfloat) width / (GLfloat) height, 1.0f, 100.0f);  // Calculate the projection matrix

    delete g_pFullScreenTexture;
    g_pFullScreenTexture = new RenderableTexture2D(GL_RGB16F,width,height);

    delete g_pPinPongTex1;
    g_pPinPongTex1 = new RenderableTexture2D(GL_RGBA16F,width,height);

    delete g_pPinPongTex2;
    g_pPinPongTex2 = new RenderableTexture2D(GL_RGBA16F,width,height);

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
  switch(key)
  {
  case 'w':
  case 'W':
    g_camPos.z -= 0.25f;
    break;

  case 's':
  case 'S':
    g_camPos.z += 0.25f;
    break;

  case 'a':
  case 'A':
   
    break;

  case 'd':
  case 'D':

    break;

  case '0':
  case ')':
    input.posprocessEffectId = 0;
    break;

  case '1':
  case '!':
    input.posprocessEffectId = 1;
    break;

  case '2':
  case '@':
    input.posprocessEffectId = 2;
    break;

  case '3':
  case '#':
    input.posprocessEffectId = 3;
    break;

  case '4':
  case '$':
    input.posprocessEffectId = 4;
    break;

  }

}

GLUSboolean update(GLUSfloat a_deltaTime)
{
  try 
  {
    static float elaspedTimeFromStart = 0.0f;
    elaspedTimeFromStart += 10*a_deltaTime;
    if(elaspedTimeFromStart > 1e5f) elaspedTimeFromStart = 0.0f;


    float3 lightPos[2];

    lightPos[0].x = 10*sin(0.15f*elaspedTimeFromStart);
    lightPos[0].y = 10*cos(0.15f*elaspedTimeFromStart);
    lightPos[0].z = 10*sin(0.15f*elaspedTimeFromStart);

    lightPos[1].x = 10*cos(0.15f*elaspedTimeFromStart);
    lightPos[1].y = 10*sin(0.15f*elaspedTimeFromStart);
    lightPos[1].z = 10*cos(0.15f*elaspedTimeFromStart);

    //lightPos[0].x = 0;
    //lightPos[0].y = 2;
    //lightPos[0].z = 10;

    //lightPos[1].x = -10;
    //lightPos[1].y = 10;
    //lightPos[1].z = -10;


    float4x4 model;
    float4x4 modelView;
    glusLoadIdentityf(model.L()); 
    glusRotateRzRyRxf(model.L(), input.cam_rot[0], input.cam_rot[1], 0.0f);
    glusLookAtf(modelView.L(), g_camPos.x, g_camPos.y, g_camPos.z, 
                               0.0f, 0.0f, 0.0f, 
                               0.0f, 1.0f, 0.0f);                           // ... and the view matrix ...

    glusMultMatrixf(modelView.L(), modelView.L(), model.L()); 	            // ... to get the final model view matrix

    if(input.posprocessEffectId != 0)
      g_pFullScreenTexture->BeginRenderingToThisTexture();

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

    glUseProgram(g_landProgram.program);

    setUniform(g_landProgram.program, "modelViewMatrix", modelView);
    setUniform(g_landProgram.program, "projectionMatrix", g_projectionMatrix);  // set matrix we have calculated in "reshape" funtion
    setUniform(g_landProgram.program, "objectMatrix", transformMatrix2);

    setUniformArray(g_landProgram.program, "g_matAmbientColors", g_matAmbientColors, 4);
    setUniformArray(g_landProgram.program, "g_matDiffuseColors", g_matDiffuseColors, 4);
    setUniformArray(g_landProgram.program, "g_matSpecularColors", g_matSpecularColors, 4);

    setUniformArray(g_landProgram.program, "g_lightPos", lightPos, 2);
    
    setUniform(g_landProgram.program, "g_camPos", g_camPos);

    bindTexture(g_landProgram.program, 1, "diffuseTexture",  g_pSnowTex->GetColorTexId());
    bindTexture(g_landProgram.program, 2, "specularTexture", g_pSnowSpecularTex->GetColorTexId());

    g_landMesh->Draw();

    // \\ land

    pParticleSystem->SetViewAndProjectionMatrices(modelView.L(), g_projectionMatrix.L());
    pParticleSystem->Process(a_deltaTime);
    pParticleSystem->Draw();

    if(input.posprocessEffectId != 0)
      g_pFullScreenTexture->EndRenderingToThisTexture();

    // post process
    //
    glDisable(GL_DEPTH_TEST);

    if(input.posprocessEffectId == 1)
    {
      glUseProgram(g_postProcessProgram.program);       
      bindTexture(g_postProcessProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());

      g_pFullScreenQuad->Draw();
    }
    else if(input.posprocessEffectId == 2)
    {
      g_pPinPongTex1->BeginRenderingToThisTexture();

      glUseProgram(g_extractBrightPixelsProgram.program); 
      bindTexture(g_extractBrightPixelsProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());
    
      g_pFullScreenQuad->Draw();

      g_pPinPongTex1->EndRenderingToThisTexture();

      //
      //
      g_pPinPongTex2->BeginRenderingToThisTexture();

      glUseProgram(g_blurProgram.program); 
      bindTexture(g_blurProgram.program, 1, "colorTexture", g_pPinPongTex1->GetColorTexId());
      setUniform  (g_blurProgram.program, "blurDirection", 0);
      setUniform  (g_blurProgram.program, "width", g_width);
      setUniform  (g_blurProgram.program, "height", g_height);
      g_pFullScreenQuad->Draw();

      g_pPinPongTex2->EndRenderingToThisTexture();
      
      //
      //
      g_pPinPongTex1->BeginRenderingToThisTexture();

      glUseProgram(g_blurProgram.program); 
      bindTexture(g_blurProgram.program, 1, "colorTexture", g_pPinPongTex2->GetColorTexId());
      setUniform  (g_blurProgram.program, "blurDirection", 1);
      setUniform  (g_blurProgram.program, "width", g_width);
      setUniform  (g_blurProgram.program, "height", g_height);

      g_pFullScreenQuad->Draw();

      g_pPinPongTex1->EndRenderingToThisTexture();

      //
      //
      glUseProgram(g_bloomProgram.program); 
      bindTexture(g_bloomProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());
      bindTexture(g_bloomProgram.program, 2, "bloomTexture", g_pPinPongTex1->GetColorTexId());

      g_pFullScreenQuad->Draw();
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

	glusPrepareContext(3, 2, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("C++, OpenGL GPU Particle System", 640, 480, GLUS_FALSE))
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
  

	// Only continue, if OpenGL 3.2 is supported.
	if (!glewIsSupported("GL_VERSION_3_2"))
	{
		printf("OpenGL 3.2 not supported.");

		glusDestroyWindow();
		return -1;
	}

  if(argc > 1)
  {
    g_fileName = std::string(argv[1]);
    std::cout << "argv[1] = " << g_fileName.c_str() << std::endl;
  }

	glusRun();

	return 0;
}

