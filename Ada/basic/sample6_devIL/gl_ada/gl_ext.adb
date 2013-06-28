with Interfaces;
with Interfaces.C;
--with Interfaces.C.Extensions;
with System;
with Ada.Text_IO;
with GL_H;

use Interfaces;
use Ada.Text_IO;
use GL_H;


-- for tracking GL_ERRORS
--
with GNAT.Traceback;           use GNAT.Traceback;
with GNAT.Traceback.Symbolic;  use GNAT.Traceback.Symbolic;

package body GL_EXT is

   procedure Call_Stack(msg : string) is
      Trace  : Tracebacks_Array (1..1_000);
      Length : Natural;
  begin
      Put_Line(msg);
      Call_Chain (Trace, Length);
      Put_Line (Symbolic_Traceback (Trace (1..Length)));
  end Call_Stack;

  procedure CHECK_GL_ERRORS is
    errCode : GLenum := glGetError;
  begin

    if errCode = GL_NO_ERROR then
      return;
    end if;

    case errCode is

      when GL_INVALID_ENUM =>

        Call_Stack("GL_INVALID_ENUM");
        raise GL_ERROR;

      when GL_INVALID_VALUE =>

        Call_Stack("GL_INVALID_VALUE");
        raise GL_ERROR;

      when GL_INVALID_OPERATION =>

        Call_Stack("GL_INVALID_OPERATION");
        raise GL_ERROR;

      when GL_STACK_OVERFLOW =>

        Call_Stack("GL_STACK_OVERFLOW");
        raise GL_ERROR;

      when GL_STACK_UNDERFLOW =>

        Call_Stack("GL_STACK_UNDERFLOW");
        raise GL_ERROR;

      when GL_OUT_OF_MEMORY =>

        Call_Stack("GL_OUT_OF_MEMORY");
        raise GL_ERROR;

      --when GL_TABLE_TOO_LARGE =>

       -- Call_Stack("GL_TABLE_TOO_LARGE");
       -- raise GL_ERROR;

      when GL_NO_ERROR =>
        null;

      when others =>

        Call_Stack("UNKNOWN_GL_ERROR");
        raise GL_ERROR;

    end case;


  end CHECK_GL_ERRORS;



  procedure glBufferData(target : GLenum; arr : GL_EXT_Integer_Array;  usage  : GLenum) is
    size : Positive       := arr'Last - arr'First + 1;
    ptr  : System.Address := arr(arr'First)'Address;
    byteSize : integer    := integer'Size/8;
  begin
    --Put("byteSize(integer) = "); Put_Line(integer'Image(byteSize));
    glBufferData(target, GLsizeiptr(size*byteSize), ptr, usage);
  end glBufferData;

  procedure glBufferData(target : GLenum; arr : GL_EXT_Float_Array;  usage  : GLenum) is
    size : Positive       := arr'Last - arr'First + 1;
    ptr  : System.Address := arr(arr'First)'Address;
    byteSize : integer    := float'Size/8;
  begin
    --Put("byteSize(float) = "); Put_Line(integer'Image(byteSize));
    --Put("size = "); Put_Line(integer'Image(size));
    glBufferData(target, GLsizeiptr(size*byteSize), ptr, usage);
  end glBufferData;



  function glGetUniformLocation(program : GLuint; name : String) return GLint is
  begin
    return glGetUniformLocationCpp(program, Interfaces.C.To_C(name));
  end glGetUniformLocation;



end GL_EXT;

