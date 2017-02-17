/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "../vsgl3/glHelper.h"
#include "../vsgl3/clHelper.h"

#include "MyOpenCLProgram.h"

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

int g_width  = 0;
int g_height = 0;

SimpleMesh* g_pLandMesh = NULL;
Texture2D*  g_pSomeTex = NULL;

float4x4    g_projectionMatrix;
float3      g_camPos(0,0,20);

MyAppCL g_app;
GLuint  g_oglBuffer1 = -1;
GLuint  g_TBO        = -1;

float g_time  = 0.0f;
int   g_index = 0;

void RequreExtentions()
{
  CHECK_GL_ERRORS;

  std::cout << "GPU Vendor: " << glGetString(GL_VENDOR) << std::endl;
  std::cout << "GPU Name  : " << glGetString(GL_RENDERER) << std::endl;
  std::cout << "GL_VER    : " << glGetString(GL_VERSION) << std::endl;
  std::cout << "GLSL_VER  : " << glGetString(GL_SHADING_LANGUAGE_VERSION) << std::endl;

}

// Функция, для привязки к шейдеру TBO - буфера через текстуру. Для обычных текстур то же самое.
//
void bindTextureBuffer(GLuint program, GLint unit, const GLchar *name, GLuint texture) 
{
  glActiveTexture(GL_TEXTURE0 + unit);       CHECK_GL_ERRORS; // делаем активным определённый текстурныю юнит/блок
  glBindTexture(GL_TEXTURE_BUFFER, texture); CHECK_GL_ERRORS; // делаем текущей текстуру отвечающую за наш буфер

  GLint location = glGetUniformLocation(program, name);   CHECK_GL_ERRORS;  // привязываем текущую текстуру (которая фикктивная) и теущим текстурным блоком к имени в шейдере
  if (location >= 0)                                                        // на следующей строчке
    glUniform1i(location, unit);                                            // да да вот тут :)
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
    g_program   = ShaderProgram("../main/Vertex.vert", "../main/Fragment.frag");
    g_pLandMesh = new SimpleMesh(g_program.program, 2, SimpleMesh::PLANE);
    g_pSomeTex  = new Texture2D("../data/texture1.bmp"); //new Texture2D("../data/snow_2_m.tga");

    g_app.Init("proc_textures.cl", 0);  // from "MyOpenCLProgram.h"

    g_app.width  = 512;
    g_app.height = 512;

    std::vector<float> data(g_app.width*g_app.height*4);
    for (size_t i = 0; i < data.size(); i += 4)
    {
      data[i + 0] = 1.0f;
      data[i + 1] = 1.0f;
      data[i + 2] = 0.25f;
      data[i + 3] = 0.0f;
    }

    int size = data.size()*sizeof(float);

    // create buffer itself
    glGenBuffers(1, &g_oglBuffer1);
    glBindBuffer(GL_ARRAY_BUFFER, g_oglBuffer1);                   CHECK_GL_ERRORS;
    glBufferData(GL_ARRAY_BUFFER, size, &data[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    g_app.RegisterGLBuffer(g_oglBuffer1);

    // create TBO
    glGenTextures(1, &g_TBO);                                      CHECK_GL_ERRORS;
    glBindTexture(GL_TEXTURE_BUFFER, g_TBO);                       CHECK_GL_ERRORS;
    glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32F, g_oglBuffer1);      CHECK_GL_ERRORS; // GL_RGBA32F means float4
    glBindTexture(GL_TEXTURE_BUFFER, 0);	                         CHECK_GL_ERRORS;

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

  case 'a':
  case 'A':
   
    break;

  case 'd':
  case 'D':

  case 'i':
  case 'I':

    break;

  }

}

GLUSboolean update(GLUSfloat time)
{
  try 
  {
    static float elaspedTimeFromStart = 0;
    elaspedTimeFromStart += 10*time;
    g_camPos.z = input.cam_dist;

    // 
    g_app.GenerateImageDataToGLBuffer(512, 512, g_index, g_time);

    g_index++;
    g_time += 0.01f;

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

    bindTexture(g_program.program      , 1, "diffuseTexture",  g_pSomeTex->GetColorTexId());
    bindTextureBuffer(g_program.program, 2, "tboSampler", g_TBO);

    g_pLandMesh->Draw();

    // \\ land
 
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
  delete g_pLandMesh; g_pLandMesh = NULL;
  delete g_pSomeTex;  g_pSomeTex = NULL;
}



/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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

	glusPrepareContext(3, 1, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("OpenCL and TBO sample", 800, 600, GLUS_FALSE))
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
	if (!glewIsSupported("GL_VERSION_3_1"))
	{
		printf("OpenGL 3.1 not supported.");

		glusDestroyWindow();
		return -1;
	}

	glusRun();

	return 0;
}

