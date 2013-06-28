#ifndef GL_HELPERS
#define GL_HELPERS


#include <iostream>
#include <string>
#include <sstream>
#include <map>
#include <string>

#include <GL/glew.h>

struct ShaderProgram // ShaderProgramCompiler
{
  ShaderProgram();

  bool Compile(const char** vert, const char** control, const char** evaluation, const char** geometry, const char** fragment);
  bool Link();

  GLuint	program;
  GLuint	vertex;
  GLuint	control;
  GLint		evaluation;
  GLuint	geometry;
  GLuint	fragment;

};


extern "C" ShaderProgram cpp_create_shader_program(const char* vs, const char* ts, const char* es, const char* gs, const char* ps);


#endif

