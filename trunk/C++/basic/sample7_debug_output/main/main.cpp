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


int g_width  = 0;
int g_height = 0;

// this is our triangle data
//
GLuint g_vertexBufferObject;
GLuint g_vertexArrayObject;

float g_projectionMatrix[16]; 


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

    // 
    //

    float trianglePos[] = 
    {
     -0.5f, -0.5f, 
      0.5f, -0.5f, 
      0.0f, +0.5f,
    };

    g_vertexBufferObject = 0;
    GLuint vertexLocation = 0; // simple layout, assume have only positions at location = 0

    glGenBuffers(1, &g_vertexBufferObject);                                                   CHECK_GL_ERRORS;
    glBindBuffer(GL_ARRAY_BUFFER, g_vertexBufferObject);                                      CHECK_GL_ERRORS;
    glBufferData(GL_ARRAY_BUFFER, 3*2*sizeof(GLfloat), (GLfloat*)trianglePos, GL_STATIC_DRAW); CHECK_GL_ERRORS;

    glGenVertexArrays(1, &g_vertexArrayObject);                                               CHECK_GL_ERRORS;
    glBindVertexArray(g_vertexArrayObject);                                                   CHECK_GL_ERRORS;

    glBindBuffer(GL_ARRAY_BUFFER, g_vertexBufferObject);                  CHECK_GL_ERRORS;                   
    glEnableVertexAttribArray(vertexLocation);                            CHECK_GL_ERRORS;
    glVertexAttribPointer(vertexLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);   CHECK_GL_ERRORS;

    glBindVertexArray(0);

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

  
  glusPerspectivef(g_projectionMatrix, 45.0f, (GLfloat) width / (GLfloat) height, 1.0f, 100.0f);  // Calculate the projection matrix
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

    // matrix setup
    //
    float model[16];
    float modelView[16];
    glusLoadIdentityf(model); 
    glusRotateRzRyRxf(model, input.cam_rot[0], input.cam_rot[1], 0.0f); // calc model matrix
    glusLookAtf(modelView, 0.0f, 0.0f, -2.0f, 
                           0.0f, 0.0f, 0.0f, 
                           0.0f, 1.0f, 0.0f);                           // ... and the view matrix ...

    glusMultMatrixf(modelView, modelView, model); 	                    // multiply them to get the final model view matrix

    // put matrices to a GPU
    {
      // put model-view matrix
      //
      GLint location = glGetUniformLocation(g_program.program, "modelViewMatrix");
      if(location >= 0)
        glUniformMatrix4fv(location, 1, GL_FALSE, (GLfloat*)&modelView);

      // put projection matrix
      //
      location = glGetUniformLocation(g_program.program, "projectionMatrix");
      if(location >= 0)
        glUniformMatrix4fv(location, 1, GL_FALSE, (GLfloat*)g_projectionMatrix);
    }

    // Now finally draw something
    //
    glViewport(0, 0, g_width, g_height);
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    
    // draw call
    //
    glBindVertexArray(g_vertexArrayObject); CHECK_GL_ERRORS;
    glDrawArrays(GL_TRIANGLES, 0, 3);       CHECK_GL_ERRORS;  // The last parameter of glDrawArrays is equal to VS invocations
  
    glBindVertexArray(-1); CHECK_GL_ERRORS; // ERROR. Debug output callback should be called at this point

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
	// Delete shader program etc..
	glusDestroyProgram(&g_program);

  glDeleteVertexArrays(1, &g_vertexArrayObject);
  glDeleteBuffers(1, &g_vertexBufferObject);
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

	if (!glusCreateWindow("C++ OpenGL sample, triangle", 640, 480, GLUS_FALSE))
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

