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

#include <vector>

ComputeProgram g_programCompute;


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

  // enable debug output
  //
  glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS_ARB);
  glDebugMessageCallbackARB(&DebugCallback, NULL);

  glDebugMessageControlARB(GL_DONT_CARE, GL_DONT_CARE, GL_DONT_CARE, 0, NULL, GL_FALSE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_ERROR_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_PERFORMANCE_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_DEPRECATED_BEHAVIOR_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_PORTABILITY_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
  glDebugMessageControlARB(GL_DONT_CARE, GL_DEBUG_TYPE_UNDEFINED_BEHAVIOR_ARB, GL_DONT_CARE, 0, NULL, GL_TRUE);
}



void computeTest(GLuint a_prog)
{
  int N = 1024;
  std::vector<float> array1(N);
  std::vector<float> array2(N);

  for(int i=0;i<array1.size();i++)
  {
    array1[i] = float(i)/10.0f;
    array2[i] = 0.0f;
  }

  // create buffers
  //
  GLuint buf1;
  glGenBuffers(1, &buf1);
  glBindBuffer(GL_SHADER_STORAGE_BUFFER, buf1);                                                     CHECK_GL_ERRORS;
  glBufferData(GL_SHADER_STORAGE_BUFFER, array1.size()*sizeof(float), &array1[0], GL_STATIC_DRAW ); CHECK_GL_ERRORS; // copy data from CPU to GPU

  GLuint buf2;
  glGenBuffers(1, &buf2);
  glBindBuffer(GL_SHADER_STORAGE_BUFFER, buf2);                                                    CHECK_GL_ERRORS;
  glBufferData(GL_SHADER_STORAGE_BUFFER, array2.size()*sizeof(float), &array2[0], GL_STATIC_DRAW); CHECK_GL_ERRORS; // copy data from CPU to GPU
  //glBufferData(GL_SHADER_STORAGE_BUFFER, array2.size()*sizeof(float), NULL, GL_STATIC_DRAW ); CHECK_GL_ERRORS; // don't do that! Seems to be an OpenGL driver bug


  // launch threads
  //
  glUseProgram(a_prog);  CHECK_GL_ERRORS;

  glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, buf1);  CHECK_GL_ERRORS; 
  glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 2, buf2);  CHECK_GL_ERRORS; 
  
  glDispatchCompute(array1.size()/256, 1, 1);     CHECK_GL_ERRORS; // 256 is a worgroup size of g_programCompute.program
  glMemoryBarrier(GL_SHADER_STORAGE_BARRIER_BIT); CHECK_GL_ERRORS; // seems not really needed but it will be better call this function before you read data

  // read back data
  //
  glBindBuffer(GL_SHADER_STORAGE_BUFFER, buf2); 
  void* resultData = glMapBufferRange(GL_SHADER_STORAGE_BUFFER, 0, array2.size()*sizeof(float), GL_MAP_READ_BIT); CHECK_GL_ERRORS;
  memcpy(&array2[0], resultData, array2.size()*sizeof(float));
  glUnmapBuffer(GL_SHADER_STORAGE_BUFFER); CHECK_GL_ERRORS;

  // print results
  //
  std::cout << "CS, output buffer values: " << std::endl;
  for(int i=0;i<10;i++)
    std::cout << array2[i] << std::endl;
  
  // delete resources
  //
  glDeleteBuffers(1, &buf1); CHECK_GL_ERRORS;
  glDeleteBuffers(1, &buf2); CHECK_GL_ERRORS;
}


/**
* Function for initialization.
*/
GLUSboolean init(GLUSvoid)
{
  try 
  {
    RequreExtentions();

    g_programCompute = ComputeProgram("../main/Compute.glsl");
    computeTest(g_programCompute.program);

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

int main(int argc, char* argv[])
{
	glusInitFunc(init);
	glusPrepareContext(4, 3, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("C++ OpenGL Compute Shaders sample", 640, 480, GLUS_FALSE))
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
  
	if (!glewIsSupported("GL_VERSION_4_3"))
	{
		printf("OpenGL 4.3 not supported.");

		glusDestroyWindow();
		return -1;
	}

	glusRun();

	return 0;
}
