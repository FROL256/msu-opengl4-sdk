with Ada.Finalization;
with Interfaces;
with Interfaces.C;
with Ada.Text_IO;
with GL_H;
with GL_EXT;

use Ada.Finalization;
use Interfaces;
use Ada.Text_IO;
use GL_H;
use GL_EXT;

package GL_HELPERS is

  -- shaders
  --
  type Shader_Program is record

    program    : GLuint;
    vertex     : GLuint;
    control    : GLuint;
    evaluation : GLuint;
    geometry   : GLuint;
    fragment   : GLuint;

  end record;

  function Create_Shader_Program(vs_path : String) return Shader_Program;
  function Create_Shader_Program(vs_path : String; ps_path : String) return Shader_Program;
  function Create_Shader_Program(vs_path : String; gs_path : String; ps_path : String) return Shader_Program;
  function Create_Shader_Program(vs_path : String; ts_path : String; es_path : String;  gs_path : String; ps_path : String) return Shader_Program;

  procedure Destroy_Shader_Program(a_prog : in out Shader_Program);


  type Full_Screen_Quad is limited private;

  procedure Draw(quad : Full_Screen_Quad);

private

  type Full_Screen_Quad is new Limited_Controlled with record
    vbo : GLuint;
    vao : GLuint;
  end record;


  procedure Initialize(self : in out Full_Screen_Quad);
  procedure Finalize(self : in out Full_Screen_Quad);

  --procedure Initialize (Self: in out Full_Screen_Quad);
  --procedure Finalize (Self: in out Full_Screen_Quad);
  --procedure Adjust(Self: in out Full_Screen_Quad);

  function cpp_create_shader_program(vs : Interfaces.C.char_array; ts : Interfaces.C.char_array; es : Interfaces.C.char_array;  gs : Interfaces.C.char_array;  ps : Interfaces.C.char_array) return Shader_Program;
  pragma import(C, cpp_create_shader_program, "cpp_create_shader_program");

end GL_HELPERS;

