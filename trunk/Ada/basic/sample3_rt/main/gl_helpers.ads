with Ada.Finalization;
with Interfaces;
with Interfaces.C;
with Ada.Text_IO;
with GL_H;
with GL_EXT;
with Vector_Math;

use Ada.Finalization;
use Interfaces;
use Ada.Text_IO;
use GL_H;
use GL_EXT;
use Vector_Math;

package GL_HELPERS is

  -- shaders
  --
  type ShaderProgram is record

    program    : GLuint;
    vertex     : GLuint;
    control    : GLuint;
    evaluation : GLuint;
    geometry   : GLuint;
    fragment   : GLuint;

  end record;

  function CreateShaderProgram(vs_path : String) return ShaderProgram;
  function CreateShaderProgram(vs_path : String; ps_path : String) return ShaderProgram;
  function CreateShaderProgram(vs_path : String; gs_path : String; ps_path : String) return ShaderProgram;
  function CreateShaderProgram(vs_path : String; ts_path : String; es_path : String;  gs_path : String; ps_path : String) return ShaderProgram;

  procedure DestroyShaderProgram(a_prog : in out ShaderProgram);


  type FullScreenQuad is limited private;
  type FullScreenQuadRef is access FullScreenQuad;

  procedure Draw(quad : FullScreenQuadRef);


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : integer);
  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float4x4);


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float3);

private

  type FullScreenQuad is new Limited_Controlled with record
    vbo : GLuint;
    vao : GLuint;
  end record;


  procedure Initialize(self : in out FullScreenQuad);
  procedure Finalize(self : in out FullScreenQuad);

  --procedure Initialize (Self: in out FullScreenQuad);
  --procedure Finalize (Self: in out FullScreenQuad);
  --procedure Adjust(Self: in out FullScreenQuad);

  function cpp_CreateShaderProgram(vs : Interfaces.C.char_array; ts : Interfaces.C.char_array; es : Interfaces.C.char_array;  gs : Interfaces.C.char_array;  ps : Interfaces.C.char_array) return ShaderProgram;
  pragma import(C, cpp_CreateShaderProgram, "cpp_create_shader_program");

end GL_HELPERS;

