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
GLUSshaderprogram g_postProcessProgram;

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
  int posprocessEffectNum;

}input;

FullScreenQuad* g_pFullScreenQuad = NULL;
SimpleMesh* g_simpleMesh = NULL;
RenderableTexture2D* g_pFullScreenTexture = NULL;

int g_width  = 0;
int g_height = 0;
float3 g_camPos(0,0,6);

float4x4 g_projectionMatrix;

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
    std::ifstream vertSourceFile("../main/Vertex.vert");
    std::ifstream geomSourceFile("../main/Geometry.geom");
    std::ifstream fragSourceFile("../main/Fragment.frag");

    std::string vertSource;
    std::string geomSource;
    std::string fragSource;

    vertSourceFile.seekg(0, std::ios::end);   
    vertSource.reserve(vertSourceFile.tellg());
    vertSourceFile.seekg(0, std::ios::beg);
    vertSource.assign((std::istreambuf_iterator<char>(vertSourceFile)), std::istreambuf_iterator<char>());

    geomSourceFile.seekg(0, std::ios::end);   
    geomSource.reserve(geomSourceFile.tellg());
    geomSourceFile.seekg(0, std::ios::beg);
    geomSource.assign((std::istreambuf_iterator<char>(geomSourceFile)), std::istreambuf_iterator<char>());

    fragSourceFile.seekg(0, std::ios::end);   
    fragSource.reserve(fragSourceFile.tellg());
    fragSourceFile.seekg(0, std::ios::beg);
    fragSource.assign((std::istreambuf_iterator<char>(fragSourceFile)), std::istreambuf_iterator<char>());


    const char* tmpVertSource = vertSource.c_str();
    const char* tmpGeomSource = geomSource.c_str();
    const char* tmpFragSource = fragSource.c_str();

    if(!glusBuildProgram(&g_program, 
                          (const GLUSchar**)&tmpVertSource,  // vertex shader source
                          NULL,                              // tessellation shader source
                          NULL,                              // evaluation shader source
                          (const GLUSchar**)&tmpGeomSource,  // geometry shader source
                          (const GLUSchar**)&tmpFragSource)) // fragment shader source
      throw std::runtime_error("shader compilation failed!");

    vertSourceFile.close();
    geomSourceFile.close();
    fragSourceFile.close();

    // load and compile shaders for post process effects
    //
    vertSourceFile.open("../main/Quad.vert");
    fragSourceFile.open("../main/Grayscale.frag");

    vertSourceFile.seekg(0, std::ios::end);   
    vertSource.reserve(vertSourceFile.tellg());
    vertSourceFile.seekg(0, std::ios::beg);
    vertSource.assign((std::istreambuf_iterator<char>(vertSourceFile)), std::istreambuf_iterator<char>());

    fragSourceFile.seekg(0, std::ios::end);   
    fragSource.reserve(fragSourceFile.tellg());
    fragSourceFile.seekg(0, std::ios::beg);
    fragSource.assign((std::istreambuf_iterator<char>(fragSourceFile)), std::istreambuf_iterator<char>());


    tmpVertSource = vertSource.c_str();
    tmpFragSource = fragSource.c_str();

    if(!glusBuildProgram(&g_postProcessProgram, 
                          (const GLUSchar**)&tmpVertSource,  // vertex shader source
                          NULL,                              // tessellation shader source
                          NULL,                              // evaluation shader source
                          NULL,                              // geometry shader source
                          (const GLUSchar**)&tmpFragSource)) // fragment shader source
      throw std::runtime_error("post process shader compilation failed!");


    g_pFullScreenQuad = new FullScreenQuad();
    g_simpleMesh = new SimpleMesh(g_program.program);

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

  glusPerspectivef(g_projectionMatrix.L(), 40.0f, (GLfloat) width / (GLfloat) height, 1.0f, 100.0f);  // Calculate the projection matrix

  delete g_pFullScreenTexture;
  g_pFullScreenTexture = new RenderableTexture2D(GL_RGBA8,width,height);
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

  input.posprocessEffectNum = 0;

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

  case '1':
  case '!':
    input.posprocessEffectNum = 1;
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

    if(input.posprocessEffectNum != 0)
      g_pFullScreenTexture->BeginRenderingToThisTexture();

    // make our program current
    //
    glUseProgram(g_program.program);  CHECK_GL_ERRORS;  

    float4x4 model;
    float4x4 modelView;
    glusLoadIdentityf(model.L()); 
    glusRotateRzRyRxf(model.L(), input.cam_rot[0], input.cam_rot[1], 0.0f);          // create world transformation matrix (rotate only in this sample)
    glusLookAtf(modelView.L(), g_camPos.x, g_camPos.y, g_camPos.z, 
                               0.0f, 0.0f, 0.0f, 
                               0.0f, 1.0f, 0.0f);                                    // and the view matrix 
    glusMultMatrixf(modelView.L(), modelView.L(), model.L()); 	                     // to get the model view matrix 

    setUniform(g_program.program, "modelViewMatrix", modelView);
    setUniform(g_program.program, "projectionMatrix", g_projectionMatrix);            // set matrix we have calculated in "reshape" funtion

    setUniform(g_program.program, "timeParameter", 0.0075f*elaspedTimeFromStart);

    // Now finally draw something
    glEnable(GL_DEPTH_TEST);
    
    glViewport(0, 0, g_width, g_height);
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    
    g_simpleMesh->Draw();

    if(input.posprocessEffectNum != 0)
      g_pFullScreenTexture->EndRenderingToThisTexture();

    if(input.posprocessEffectNum == 1)
    {
      glUseProgram(g_postProcessProgram.program); 
      
      bindTexture(g_postProcessProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());

      glDisable(GL_DEPTH_TEST);
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
 * Function to clean up things.
 */
GLUSvoid shutdown(GLUSvoid)
{
  delete g_pFullScreenQuad; g_pFullScreenQuad = NULL;
  delete g_simpleMesh; g_simpleMesh = NULL;

	// Delete shader program etc..
	glusDestroyProgram(&g_program);
  glusDestroyProgram(&g_postProcessProgram);
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

	if (!glusCreateWindow("C++, Simple GS + Post Process", 640, 480, GLUS_FALSE))
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

