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

ShaderProgram  g_programDisplay;
ShaderProgram  g_programCopy;
ShaderProgram  g_programLoadStore;
ComputeProgram g_programLoadStoreCompute;

struct MyInput
{
  MyInput() 
  {
    cam_rot[0] = cam_rot[1] = cam_rot[2] = cam_rot[3] = 0.f;
    mx = my = 0;
    rdown = ldown = false;
	  cam_rot[0] = 75;
    cam_dist = 30.0f;
    fs_or_cs = 1;
  }

  int mx;
  int my;
  bool rdown;
  bool ldown;
  float cam_rot[4];
  float cam_pos[4];

  float cam_dist;

  int fs_or_cs;

}input;

FullScreenQuad* g_pFullScreenQuad = NULL;
int g_width  = 0;
int g_height = 0;

RenderableTexture2D* actionTexture = NULL;

SimpleMesh* plane_mesh = NULL;
Texture2D*  inputTexture = NULL;

float4x4 g_projectionMatrix;

float3 g_camPos (0,0,20);
bool g_computeShadersSupported = false;


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
  h.require("GL_ARB_debug_output");
  h.require("GL_EXT_shader_image_load_store");

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

    g_programDisplay    = ShaderProgram("../main/Vertex.vert", "../main/Fragment.frag");
    g_programCopy       = ShaderProgram("../main/Quad.vert", "../main/Copy.frag");
    g_programLoadStore  = ShaderProgram("../main/Quad.vert", "../main/LoadStore.frag");

    if(g_computeShadersSupported)
      g_programLoadStoreCompute = ComputeProgram("../main/LoadStoreCompute.glsl");

    g_pFullScreenQuad  = new FullScreenQuad();
    plane_mesh         = new SimpleMesh(g_programDisplay.program, 2, SimpleMesh::PLANE);
    inputTexture       = new Texture2D("../data/crate.tga");

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

  actionTexture = new RenderableTexture2D(GL_RGBA32F, 256, 256);
}

GLUSvoid mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y)
{
  if(!pressed) return;

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
    input.cam_dist -= 0.1f;
    break;

  case 's':
  case 'S':
    input.cam_dist += 0.1f;
    break;

  case '1':
  case '!':
    input.fs_or_cs = 1;
    break;

  case '2':
  case '@':
    input.fs_or_cs = 2;
    break;

  }
}

float anim = 0;

GLUSboolean update(GLUSfloat time)
{
  try 
  {
    g_camPos.z = input.cam_dist;
	  anim += time;

    float4x4 model;
    float4x4 modelView;
    glusLoadIdentityf(model.L()); 
    glusRotateRzRyRxf(model.L(), input.cam_rot[0], input.cam_rot[1], 0.0f);
    glusLookAtf(modelView.L(), g_camPos.x, g_camPos.y, g_camPos.z, 
                               0.0f, 0.0f, 0.0f, 
                               0.0f, 1.0f, 0.0f);                           // ... and the view matrix ...

    glusMultMatrixf(modelView.L(), modelView.L(), model.L()); 	            // ... to get the final model view matrix
	
    // copy data to our RGBA32F texture
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    glDisable(GL_DEPTH_TEST);
    glDisable(GL_BLEND);

	  glViewport(0, 0, 256, 256);
    actionTexture->BeginRenderingToThisTexture();
    glUseProgram(g_programCopy.program);
    bindTexture(g_programCopy.program, 1, "inTex", inputTexture->GetColorTexId());
    g_pFullScreenQuad->Draw(); CHECK_GL_ERRORS;                                                            
    actionTexture->EndRenderingToThisTexture();

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
     
    // using load/store from pixel shader
    //
    if(input.fs_or_cs == 1)
    {
      glViewport(0, 0, 256, 256);
      glUseProgram(g_programLoadStore.program);

      setUniform (g_programLoadStore.program, "time", anim);
      glActiveTexture (GL_TEXTURE0);																		
      glBindImageTextureEXT(0, actionTexture->GetColorTexId(), 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32F); CHECK_GL_ERRORS;

      GLint imageLoc = glGetUniformLocation(g_programLoadStore.program, "image");				
      if(imageLoc >= 0)
        glUniform1i(imageLoc, 0);			 														

      g_pFullScreenQuad->Draw(); CHECK_GL_ERRORS;
    }
    else if(g_computeShadersSupported)
    {
      glUseProgram(g_programLoadStoreCompute.program);

      setUniform (g_programLoadStoreCompute.program, "time", anim);
      glActiveTexture (GL_TEXTURE0);																		
      glBindImageTextureEXT(0, actionTexture->GetColorTexId(), 0, GL_FALSE, 0, GL_READ_WRITE, GL_RGBA32F); CHECK_GL_ERRORS;

      GLint imageLoc = glGetUniformLocation(g_programLoadStoreCompute.program, "image");				
      if(imageLoc >= 0)
        glUniform1i(imageLoc, 0);		
   
      glDispatchCompute(256/16, 256/16, 1); 
    }
    else
      std::cout << "compute shaders are not supported!" << std::endl;
    
    // generate mips if needed
    //
    glBindTexture(GL_TEXTURE_2D, actionTexture->GetColorTexId());
    glGenerateMipmap(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, 0);
	  
    /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    // draw plane with texture
    //
    glViewport(0, 0, g_width, g_height);

    float4x4 rotationMatrix, scaleMatrix, translateMatrix;
    float4x4 transformMatrix1, transformMatrix2;

    glusRotateRzRyRxf(rotationMatrix.L(), 90, 0.0f, 0.0f);
    glusScalef(scaleMatrix.L(), 10, 10, 10);
    glusTranslatef(translateMatrix.L(), 0,-1,0);
    glusMultMatrixf(transformMatrix1.L(), rotationMatrix.L(), scaleMatrix.L());
    glusMultMatrixf(transformMatrix2.L(), translateMatrix.L(), transformMatrix1.L());

    glUseProgram(g_programDisplay.program);

    setUniform(g_programDisplay.program, "modelViewMatrix", modelView);
    setUniform(g_programDisplay.program, "projectionMatrix", g_projectionMatrix);  // set matrix we have calculated in "reshape" funtion
    setUniform(g_programDisplay.program, "objectMatrix", transformMatrix2);

    bindTexture(g_programDisplay.program, 1, "diffuseTexture",  actionTexture->GetColorTexId());

    plane_mesh->Draw();

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
  delete plane_mesh; plane_mesh = NULL;

  delete inputTexture; inputTexture = NULL;
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

  const char* header = "image load/store (press 1 or 2 to select between fragment and compute shaders version)";

  glusPrepareContext(4, 3, GLUS_FORWARD_COMPATIBLE_BIT);
  if (!glusCreateWindow(header, 640, 480, GLUS_FALSE))
  {
    glusPrepareContext(4, 2, GLUS_FORWARD_COMPATIBLE_BIT);
    if (!glusCreateWindow(header, 640, 480, GLUS_FALSE))
    {
      printf("Could not create window!");
      return -1;
    }
  }
  else
    g_computeShadersSupported = true;

  // Init GLEW
  glewExperimental = GL_TRUE;
  GLenum err=glewInit();
  if(err!=GLEW_OK)
  {
    sprintf("glewInitError", "Error: %s\n", glewGetErrorString(err));
    return -1;
  }
  glGetError(); // flush error state variable, caused by glew errors


  if (!glewIsSupported("GL_VERSION_4_3"))
  {
    g_computeShadersSupported = false;
    if (!glewIsSupported("GL_VERSION_4_2"))
    {
      printf("OpenGL 4.2 not supported.");
      glusDestroyWindow();
      return -1;
    }
  }

  glusRun();

  return 0;
}

