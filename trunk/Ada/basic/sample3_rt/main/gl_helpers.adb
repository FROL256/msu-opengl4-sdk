with Interfaces;
with Interfaces.C;
with Ada.Text_IO;
with GL_H;
with GL_EXT;

use Interfaces;
use Interfaces.C;
use Ada.Text_IO;
use GL_H;
use GL_EXT;

package body GL_HELPERS is


  function CreateShaderProgram(vs_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(""), To_C(""), To_C(""), To_C(""));
  end CreateShaderProgram;

  function CreateShaderProgram(vs_path : String; ps_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(""), To_C(""), To_C(""), To_C(ps_path));
  end CreateShaderProgram;

  function CreateShaderProgram(vs_path : String; gs_path : String; ps_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(""), To_C(""), To_C(gs_path), To_C(ps_path));
  end CreateShaderProgram;

  function CreateShaderProgram(vs_path : String; ts_path : String; es_path : String; gs_path : String; ps_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(ts_path), To_C(es_path), To_C(gs_path), To_C(ps_path));
  end CreateShaderProgram;



   procedure DestroyShaderProgram(a_prog : in out ShaderProgram) is

   begin

     if a_prog.program > 0 then

      glDeleteProgram(a_prog.program);

      if a_prog.vertex > 0 then
        glDeleteShader(a_prog.vertex);
      end if;

      if a_prog.control > 0 then
        glDeleteShader(a_prog.control);
      end if;

      if a_prog.evaluation > 0 then
        glDeleteShader(a_prog.evaluation);
      end if;

      if a_prog.geometry > 0 then
        glDeleteShader(a_prog.geometry);
      end if;

      if a_prog.fragment > 0 then
        glDeleteShader(a_prog.fragment);
      end if;

     end if;


  end DestroyShaderProgram;

  procedure Initialize(self : in out FullScreenQuad) is
    vertexLocation : GLuint := 0;
    quadPos : array (1 .. 8) of float;
  begin
    Put_Line("FS_Quad_Init");
    self.vbo := 0;
    self.vao := 0;
    quadPos  := (-1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 1.0, -1.0);

    self.vbo := glGenBuffer;
    glBindBuffer(GL_ARRAY_BUFFER, self.vbo);                                    CHECK_GL_ERRORS;
    glBufferData(GL_ARRAY_BUFFER, GL_EXT_Float_Array(quadPos), GL_STATIC_DRAW); CHECK_GL_ERRORS;

    self.vao := glGenVertexArray;
    glBindVertexArray(self.vao);

    glBindBuffer(GL_ARRAY_BUFFER, self.vbo);                                    CHECK_GL_ERRORS;
    glEnableVertexAttribArray(vertexLocation);                                  CHECK_GL_ERRORS;
    glVertexAttribPointer(vertexLocation, 2, GL_FLOAT, GLboolean(GL_FALSE), 0); CHECK_GL_ERRORS;

  end Initialize;


  procedure Finalize(self : in out FullScreenQuad) is
  begin
    Put_Line("FS_Quad_Destoy");

    if self.vbo > 0 then
      glDeleteBuffer(self.vbo);
    end if;

    if self.vao > 0 then
      glDeleteVertexArray(self.vao);
    end if;

  end Finalize;

  procedure Draw(quad : FullScreenQuadRef) is
  begin

    glBindVertexArray(quad.vao);            CHECK_GL_ERRORS;
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);  CHECK_GL_ERRORS;

  end Draw;


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : integer) is
    loc : GLint := glGetUniformLocation(a_prog, a_name);
  begin
    if loc >= 0 then
      glUniform1i(loc, a_val);
    end if;
  end setUniform;


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float4x4) is
   loc : GLint    := glGetUniformLocation(a_prog, a_name);
   mt  : float4x4 := transpose(a_val);
  begin
    if loc >= 0 then
      glUniformMatrix4fv(loc, 1, GLBoolean(GL_FALSE), mt(0,0)'address);
    end if;
  end setUniform;


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float3) is
    loc : GLint := glGetUniformLocation(a_prog, a_name);
  begin
     if loc >= 0 then
      glUniform3f(loc, a_val.x, a_val.y, a_val.z);
    end if;
  end setUniform;

end GL_HELPERS;

