with Interfaces;
with Interfaces.C;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;
with System;
with Ada.Text_IO;
with GL_H;

use Interfaces;
use Ada.Text_IO;
use GL_H;

package GL_EXT is

  subtype GLsizeiptr is GLuint; --

  GL_TEXTURE0 : constant GLuint := 33984;

  GL_ERROR: exception;
  procedure CHECK_GL_ERRORS;

  -- program
  --
  procedure glUseProgram(a_shader : GLuint);     pragma Import(C, glUseProgram, "cpp_glUseProgram");
  procedure glDeleteShader(a_shader : GLuint);   pragma Import(C, glDeleteShader, "cpp_glDeleteShader");
  procedure glDeleteProgram(a_shader : GLuint);  pragma Import(C, glDeleteProgram, "cpp_glDeleteProgram");

  -- buffers
  --
  function  glGenBuffer return Gluint; pragma Import(C, glGenBuffer, "cpp_glGenBuffer");
  procedure glBindBuffer(a_target : Glenum; a_buffer : GLuint);   pragma Import(C, glBindBuffer, "cpp_glBindBuffer");

  procedure glBufferData(target : GLenum;
                         size   : GLsizeiptr;
                         data   : System.Address;
                         usage  : GLenum);

  pragma Import(C, glBufferData, "cpp_glBufferData");

  -- now add 2 more glBufferData functions
  --
  type GL_EXT_Integer_Array is array (Integer range <>) of aliased Integer;
  type GL_EXT_Float_Array   is array (Integer range <>) of aliased Float;

  type GL_EXT_Float_Var_Ptr is access all Float;
  type GL_EXT_Integer_Var_Ptr is access all Integer;


  procedure glBufferData(target : GLenum; arr : GL_EXT_Integer_Array;  usage  : GLenum);
  procedure glBufferData(target : GLenum; arr : GL_EXT_Float_Array;  usage  : GLenum);

  procedure glDeleteBuffer(a_buffer : GLuint);   pragma Import(C, glDeleteBuffer, "cpp_glDeleteBuffer");



  -- vertex arrays
  --
  function  glGenVertexArray return GLuint;          pragma Import(C, glGenVertexArray, "cpp_glGenVertexArray");
  procedure glBindVertexArray(vao : GLuint);         pragma Import(C, glBindVertexArray, "cpp_glBindVertexArray");

  procedure glVertexAttribPointer(index  : GLuint;
                                  size   : GLint;
                                  a_type : GLenum;
                                  normalized : GLboolean;
                                  stride : GLsizei);

  pragma Import(C, glVertexAttribPointer, "cpp_glVertexAttribPointer2");

  procedure glEnableVertexAttribArray(loc : GLuint); pragma Import(C, glEnableVertexAttribArray, "cpp_glEnableVertexAttribArray");
  procedure glDeleteVertexArray(vao : GLuint);       pragma Import(C, glDeleteVertexArray, "cpp_glDeleteVertexArray");


  -- uniforms
  --
  function  glGetUniformLocation(program : GLuint; name : String) return GLint;

  function  glGetUniformLocationCpp(program : GLuint; name : Interfaces.C.char_array) return GLint;
  pragma Import(C, glGetUniformLocationCpp, "cpp_glGetUniformLocation");


  procedure glUniform1i(location : GLint; v0 : GLint);
  procedure glUniform1f(location : GLint; v0 : GLfloat);
  procedure glUniform2f(location : GLint; v0 : GLfloat; v1 : GLfloat);
  procedure glUniform3f(location : GLint; v0 : GLfloat; v1 : GLfloat; v2 : GLfloat);
  procedure glUniform4f(location : GLint; v0 : GLfloat; v1 : GLfloat; v2 : GLfloat; v3 : GLfloat);

  procedure glUniformMatrix4fv(location : GLint; count : GLsizei; transpose : GLboolean; value : System.Address);

  procedure glActiveTexture(unit : GLuint);

  pragma Import(C, glUniform1i, "cpp_glUniform1i");
  pragma Import(C, glUniform1f, "cpp_glUniform1f");
  pragma Import(C, glUniform2f, "cpp_glUniform2f");
  pragma Import(C, glUniform3f, "cpp_glUniform3f");
  pragma Import(C, glUniform4f, "cpp_glUniform4f");
  pragma Import(C, glUniformMatrix4fv, "cpp_glUniformMatrix4fv");

  pragma Import(C, glActiveTexture, "cpp_glActiveTexture");

  GL_ARRAY_BUFFER : GLenum := 34962;
  GL_STATIC_DRAW  : GLenum := 35044;

  GL_ELEMENT_ARRAY_BUFFER : GLenum := 34963;


  NullPointer : aliased GLvoid;
  pragma Import(C, NullPointer, "cpp_nullPointer");

private

end GL_EXT;


