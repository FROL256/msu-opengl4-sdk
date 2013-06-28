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

extern "C" GLuint cpp_create_gl_texture_from_file(const char* a_fileName);
extern "C" void cpp_save_gl_texture_to_file(const char* a_fileName,  GLuint a_texId, GLenum a_format, int mipLevel);

#endif

