#pragma once

//////////////////////////////////////////////////////////////////
// glHelper.h Author: Vladimir Frolov, 2011, Graphics & Media Lab.
//////////////////////////////////////////////////////////////////

#include <iostream>
#include <string>
#include <sstream>
#include <map>

#include <GL/glew.h>

struct GlusHelperRequireExt
{
  GlusHelperRequireExt() 
  {
    int n=0;
    glGetIntegerv(GL_NUM_EXTENSIONS, &n);  
    for(int i=0;i<n;i++)
    {
      const char*  extensions = (const char*)glGetStringi(GL_EXTENSIONS,i);
      supportedExtensions[extensions] = true;
    }
  }

  void require(const std::string& extName)
  {
    if (supportedExtensions.find(extName) == supportedExtensions.end())
      throw std::runtime_error("unsupported extension: " + extName);
  }

  std::map<std::string, bool> supportedExtensions;
};

static std::string DecodeGLError(int line, const char *file)
{
  std::ostringstream errMsg;

  GLenum gl_error = glGetError();

  switch(gl_error)
  {
  case GL_INVALID_ENUM:
    errMsg << "GL_INVALID_ENUM in " << file << ", line " << line;
    break;

  case GL_INVALID_VALUE:
    errMsg << "GL_INVALID_VALUE in " << file << ", line " << line;
    break;

  case GL_INVALID_OPERATION:
    errMsg << "GL_INVALID_OPERATION in " << file << ", line " << line;
    break;

  case GL_STACK_OVERFLOW:
    errMsg << "GL_STACK_OVERFLOW in " << file << ", line " << line;
    break;

  case GL_STACK_UNDERFLOW:
    errMsg << "GL_STACK_UNDERFLOW in " << file << ", line " << line;
    break;

  case GL_OUT_OF_MEMORY:
    errMsg << "GL_OUT_OF_MEMORY in " << file << ", line " << line;
    break;

  case GL_TABLE_TOO_LARGE:
    errMsg << "GL_TABLE_TOO_LARGE in " << file << ", line " << line;
    break;

  case GL_NO_ERROR:
    errMsg << "GL_NO_ERROR in " << file << ", line " << line;
    break;

  default:
    errMsg << "UNKNOWN GL ERROR in " << file << ", line " << line;
    break;
  }

  return errMsg.str();
}

#pragma warning(disable:4996)

static void ThrowExceptionOnGLError(int line, const char *file)
{
  static char errMsg[512];

  GLenum gl_error = glGetError();

  if(gl_error == GL_NO_ERROR)
    return;

  switch(gl_error)
  {
  case GL_INVALID_ENUM:
    sprintf(errMsg, "GL_INVALID_ENUM file %s line %d\n", file, line);
    break;

  case GL_INVALID_VALUE:
    sprintf(errMsg, "GL_INVALID_VALUE file %s line %d\n",  file, line);
    break;

  case GL_INVALID_OPERATION:
    sprintf(errMsg, "GL_INVALID_OPERATION file %s line %d\n",  file, line);
    break;

  case GL_STACK_OVERFLOW:
    sprintf(errMsg, "GL_STACK_OVERFLOW file %s line %d\n",  file, line);
    break;

  case GL_STACK_UNDERFLOW:
    sprintf(errMsg, "GL_STACK_UNDERFLOW file %s line %d\n",  file, line);
    break;

  case GL_OUT_OF_MEMORY:
    sprintf(errMsg, "GL_OUT_OF_MEMORY file %s line %d\n",  file, line);
    break;

  case GL_TABLE_TOO_LARGE:
    sprintf(errMsg, "GL_TABLE_TOO_LARGE file %s line %d\n",  file, line);
    break;

  case GL_NO_ERROR:
    break;

  default:
    sprintf(errMsg, "Unknown error @ file %s line %d\n",  file, line);
    break;
  }

  if(gl_error != GL_NO_ERROR)
    throw std::runtime_error(errMsg);
}




#undef CHECK_GL_ERRORS
#define CHECK_GL_ERRORS ThrowExceptionOnGLError(__LINE__,__FILE__)

