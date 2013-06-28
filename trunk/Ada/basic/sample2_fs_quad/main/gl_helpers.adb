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


  function Create_Shader_Program(vs_path : String) return Shader_Program is
  begin
    return cpp_create_shader_program(To_C(vs_path), To_C(""), To_C(""), To_C(""), To_C(""));
  end Create_Shader_Program;

  function Create_Shader_Program(vs_path : String; ps_path : String) return Shader_Program is
  begin
    return cpp_create_shader_program(To_C(vs_path), To_C(""), To_C(""), To_C(""), To_C(ps_path));
  end Create_Shader_Program;

  function Create_Shader_Program(vs_path : String; gs_path : String; ps_path : String) return Shader_Program is
  begin
    return cpp_create_shader_program(To_C(vs_path), To_C(""), To_C(""), To_C(gs_path), To_C(ps_path));
  end Create_Shader_Program;

  function Create_Shader_Program(vs_path : String; ts_path : String; es_path : String; gs_path : String; ps_path : String) return Shader_Program is
  begin
    return cpp_create_shader_program(To_C(vs_path), To_C(ts_path), To_C(es_path), To_C(gs_path), To_C(ps_path));
  end Create_Shader_Program;



   procedure Destroy_Shader_Program(a_prog : in out Shader_Program) is

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


  end Destroy_Shader_Program;

  procedure Initialize(self : in out Full_Screen_Quad) is
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


  procedure Finalize(self : in out Full_Screen_Quad) is
  begin
    Put_Line("FS_Quad_Destoy");

    if self.vbo > 0 then
      glDeleteBuffer(self.vbo);
    end if;

    if self.vao > 0 then
      glDeleteVertexArray(self.vao);
    end if;

  end Finalize;

  procedure Draw(quad : Full_Screen_Quad) is
  begin

    glBindVertexArray(quad.vao);            CHECK_GL_ERRORS;
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);  CHECK_GL_ERRORS;

  end Draw;


end GL_HELPERS;

