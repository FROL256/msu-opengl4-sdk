#ifdef  CPP_GL_EXT_GUARDIAN
#define CPP_GL_EXT_GUARDIAN

#include "GL/glew.h"
#include "GL/glus.h"

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>


extern "C" void cpp_glUseProgram(GLuint a_program);
extern "C" void cpp_glDeleteShader(GLuint a_shader);
extern "C" void cpp_glDeleteProgram(GLuint a_program);

extern "C" GLuint cpp_glGenBuffer();
extern "C" void cpp_glBindBuffer(GLenum target, GLuint buffer);
extern "C" void cpp_glBufferData(GLenum target, GLsizeiptr size, const GLvoid*  data, GLenum  usage);
extern "C" void cpp_glDeleteBuffer(GLuint buffer);


extern "C" GLuint cpp_glGenVertexArray();
extern "C" void cpp_glBindVertexArray(GLuint a_array);
extern "C" void cpp_glEnableVertexAttribArray(GLuint a_loc);
extern "C" void cpp_glVertexAttribPointer2(GLuint  index, GLint  size, GLenum  type, GLboolean  normalized,  GLsizei  stride); // hide last parameter, it sounds useless
extern "C" void cpp_glDeleteVertexArray(GLuint a_vao);



#endif

