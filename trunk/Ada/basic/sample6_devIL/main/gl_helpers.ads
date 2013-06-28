with Ada.Finalization;
with Interfaces;
with Interfaces.C;
with Ada.Text_IO;
with GL_H;
with GL_EXT;
with Vector_Math;
with System;

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

  --
  --
  function  CreateGLTextureFromFile(a_fileName : String) return GLuint;
  procedure DestroyGLTexture(a_texId : in out GLuint);

  procedure bindTexture(program : GLuint;  unit : GLint; name : String; texture : GLuint);

  --
  --
  type FullScreenQuad is limited private;
  type FullScreenQuadRef is access FullScreenQuad;

  type GeometryShape is limited private;
  type GeometryShapeRef is access GeometryShape;

  function CreateShapePlane(sizeX : float; sizeY : float; subdivsX : integer; subdivsY : integer) return GeometryShapeRef;
  procedure Draw(shape : GeometryShapeRef);

  procedure Draw(quad : FullScreenQuadRef);

  procedure setUniform(a_prog : GLuint; a_name : String; a_val : integer);
  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float);
  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float4x4);
  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float3);

  function ProjectionMatrix(fovy : float; aspect : float; zNear : float; zFar : float) return float4x4;
  function LookAtMatrixGLUS(eye : float3; center : float3; upVector : float3 ) return float4x4;

  procedure PrintMatrix(mat : float4x4);

private

  type FullScreenQuad is new Limited_Controlled with record
    vbo : GLuint;
    vao : GLuint;
  end record;

  procedure Initialize(self : in out FullScreenQuad);
  procedure Finalize(self : in out FullScreenQuad);

  type GeometryShape is new Limited_Controlled with record
    vbo_pos  : GLuint  := 0;
    vbo_norm : GLuint  := 0;
    vbo_tex  : GLuint  := 0;
    vao      : GLuint  := 0;
    vert_num : integer := 0;
  end record;

  procedure Initialize(self : in out GeometryShape);
  procedure Finalize(self : in out GeometryShape);


  function cpp_CreateShaderProgram(vs : Interfaces.C.char_array; ts : Interfaces.C.char_array; es : Interfaces.C.char_array;  gs : Interfaces.C.char_array;  ps : Interfaces.C.char_array) return ShaderProgram;
  pragma import(C, cpp_CreateShaderProgram, "cpp_create_shader_program");

  procedure glusPerspectivef(result : System.Address; fovy : FLOAT; aspect : FLOAT; zNear : FLOAT; zFar : FLOAT);
  pragma import(C, glusPerspectivef, "glusPerspectivef");

  procedure glusLookAtf(result : System.Address;
                        eyeX : FLOAT; eyeY : FLOAT; eyeZ : FLOAT;
                        centerX : FLOAT; centerY : FLOAT; centerZ : FLOAT;
                        upX : FLOAT; upY : FLOAT; upZ : FLOAT);

  pragma import(C, glusLookAtf, "glusLookAtf");


  function cpp_create_gl_texture_from_file(a_fileName : Interfaces.C.char_array) return GLuint;
  pragma import(C, cpp_create_gl_texture_from_file, "cpp_create_gl_texture_from_file");

  procedure cpp_save_gl_texture_to_file( a_fileName : Interfaces.C.char_array; a_texId : GLuint; a_format : GLenum; mipLevel : integer);
  pragma import(C, cpp_save_gl_texture_to_file, "cpp_save_gl_texture_to_file");

end GL_HELPERS;



