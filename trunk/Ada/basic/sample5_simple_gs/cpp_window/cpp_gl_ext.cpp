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
extern "C" void cpp_glVertexAttribPointer2(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride) { glVertexAttribPointer(index,size,type,normalized,stride, 0); }
extern "C" void cpp_glDeleteVertexArray(GLuint a_vao) { glDeleteVertexArrays(1, &a_vao); }


extern "C" GLint cpp_glGetUniformLocation(GLuint program, const char* name) { return glGetUniformLocation(program, name); }
extern "C" void cpp_glUniform1i(GLint location, GLint v0) { glUniform1i(location,v0); }

extern "C" void cpp_glUniform1f(GLint location, GLfloat v0) { glUniform1f(location,v0); }
extern "C" void cpp_glUniform2f(GLint location, GLfloat v0, GLfloat v1) { glUniform2f(location,v0,v1); }
extern "C" void cpp_glUniform3f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2) { glUniform3f(location,v0,v1,v2); }
extern "C" void cpp_glUniform4f(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3) { glUniform4f(location,v0,v1,v2,v3); }


void debugPrintMat(const GLfloat* value)
{
  //FILE* outFile = fopen("matrices.txt", "a");

  for(int x=0;x<4;x++)
  {
      for(int y=0;y<4;y++)
      {
          fprintf(stderr, "%f ", value[y*4+x]);
      }
      fprintf(stderr, "\n");
    }

  //fclose(outFile);
}

extern "C" void cpp_glUniformMatrix4fv(GLint location, GLsizei count, GLboolean transpose, const GLfloat* value)
{
  //debugPrintMat(value);
  glUniformMatrix4fv(location, count, transpose, value);
}

