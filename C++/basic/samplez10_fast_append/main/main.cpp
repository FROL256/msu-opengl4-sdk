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

#include "SharedBuffer.h"


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


// this should be implemented for each special case of data layout
//
GLuint CreateVAOForSpecialCase_Data1fAndMask1i(GLuint a_dataBuffer, GLuint a_maskBuffer, int locData, int locMask)
{
  GLuint vao;

  glGenVertexArrays(1, &vao); CHECK_GL_ERRORS;
  glBindVertexArray(vao);     CHECK_GL_ERRORS;

  // bind data buffer
  //
  if(locData >= 0)
  {
    glBindBuffer(GL_ARRAY_BUFFER, a_dataBuffer);                        CHECK_GL_ERRORS;                   
    glEnableVertexAttribArray(locData);                                 CHECK_GL_ERRORS;
    glVertexAttribPointer(locData, 1, GL_FLOAT, GL_FALSE, 0, 0);        CHECK_GL_ERRORS;
  }

  // bind mask buffer
  //
  if(locMask >= 0)
  {
    glBindBuffer(GL_ARRAY_BUFFER, a_maskBuffer);                        CHECK_GL_ERRORS;                   
    glEnableVertexAttribArray(locMask);                                 CHECK_GL_ERRORS;
    glVertexAttribPointer(locMask, 1, GL_UNSIGNED_INT, GL_FALSE, 0, 0); CHECK_GL_ERRORS;
  }

  glBindVertexArray(0);             CHECK_GL_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, 0); CHECK_GL_ERRORS;

  return vao;
}



/**
* Function for initialization.
*/
GLUSboolean init(GLUSvoid)
{
  try 
  {
    RequreExtentions();


    // this shows how to append with transform feedback
    //
    std::cout << std::endl;
    std::cout << "begin append test sample" << std::endl;
    {
      const int N = 10;
      float data[N]  = {0,1,2,3,4,5,6,7,8,9};
      float data2[N] = {10,11,12,13,14,15,16,17,18,19};
      int mask[N]    = {0,0,0,1,1,0,0,0,1,0};

      SharedBuffer dataBuff(data, N*sizeof(float));
      SharedBuffer maskBuff(mask, N*sizeof(int));
      SharedBuffer resBuff(data2, N*sizeof(float));

      // (1) Create and relink(!!!) program
      //
      ShaderProgram appendProg("../main/Append.vert", "../main/Append.geom", "../main/Append.frag");

      // set output name and relink shader. Please note this step is very important!!!!!!!!!!!
      //
      const GLchar* names[1] = {"outData"};
      glTransformFeedbackVaryings (appendProg.program, 1, names, GL_SEPARATE_ATTRIBS); CHECK_GL_ERRORS;
      if(!appendProg.Link())
        std::cerr << "can not relink program after glTransformFeedbackVaryings" << std::endl;

      // create input VAO
      //
      int dataLoc = glGetAttribLocation(appendProg.program, "vertex");
      int maskLoc = glGetAttribLocation(appendProg.program, "inMask");

      GLuint sourceVAO = CreateVAOForSpecialCase_Data1fAndMask1i(dataBuff.mapToGL(), maskBuff.mapToGL(), dataLoc, maskLoc);

      // do append
      //
      GLuint query;
      {
        glGenQueries(1, &query);
        glBeginQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN, query); // Then, right before calling glBeginTransformFeedback, you have to tell OpenGL to keep track of the amount of primitives written

        AppendDataWithProgramFromVAO(sourceVAO, resBuff.mapToGL(), N, appendProg.program);      

        glEndQuery(GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN);
      }

      // this tells how many primitives were written
      //
      GLuint dataWritten = 0;
      glGetQueryObjectuiv(query, GL_QUERY_RESULT, &dataWritten);

      //
      //
      const float* resData = (const float*)resBuff.mapToCPUForRead(); // generate performance warning - for this case it is fine
      
      std::cout << std::endl;
      std::cout << "written numbers : " << dataWritten << std::endl;
      for(int i=0;i<dataWritten;i++)
        std::cout << resData[i] << " ";
      std::cout << std::endl;

      glDeleteVertexArrays(1, &sourceVAO); CHECK_GL_ERRORS;
    }
    std::cout << "end append test sample" << std::endl;
    std::cout << std::endl;



    // and here should be the test with append and OpenGL
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

int main(int argc, char* argv[])
{
	glusInitFunc(init);
	glusPrepareContext(3, 2, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("C++ OpenGL Append With Transform Feedback", 640, 480, GLUS_FALSE))
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
  
	if (!glewIsSupported("GL_VERSION_3_2"))
	{
		printf("OpenGL 3.2 not supported.");

		glusDestroyWindow();
		return -1;
	}

	glusRun();

	return 0;
}
