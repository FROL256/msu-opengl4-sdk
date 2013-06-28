#include "GL/glew.h"
#include "cpp_gl_ext.h"

#include <stdio.h>

extern "C" void cpp_glUseProgram(GLuint a_program)    { glUseProgram(a_program); }
extern "C" void cpp_glDeleteShader(GLuint a_shader)   { glDeleteShader(a_shader); }
extern "C" void cpp_glDeleteProgram(GLuint a_program) { glDeleteProgram(a_program); }


extern "C" GLuint cpp_glGenBuffer() { GLuint vbo; glGenBuffers(1, &vbo); return vbo; }

extern "C" void cpp_glBindBuffer(GLenum target, GLuint buffer) { glBindBuffer(target, buffer); }
extern "C" void cpp_glBufferData(GLenum target, GLsizeiptr size, const GLvoid*  data, GLenum  usage) { glBufferData(target, size, data, usage);  }
extern "C" void cpp_glDeleteBuffer(GLuint buffer) { glDeleteBuffers(1, &buffer); }


extern "C" GLuint cpp_glGenVertexArray() { GLuint vao; glGenVertexArrays(1, &vao); return vao; }
extern "C" void cpp_glBindVertexArray(GLuint a_array) { glBindVertexArray(a_array); }
extern "C" void cpp_glEnableVertexAttribArray(GLuint a_loc) { glEnableVertexAttribArray(a_loc); }
extern "C" void cpp_glVertexAttribPointer2(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const GLvoid* pointer) { glVertexAttribPointer(index,size,type,normalized,stride,NULL); }
extern "C" void cpp_glDeleteVertexArray(GLuint a_vao) { glDeleteVertexArrays(1, &a_vao); }



