with Interfaces;
with Ada.Text_IO;
with System;
with Interfaces.C;
with Interfaces.C.Strings;


use Interfaces;
use Ada.Text_IO;
use Interfaces.C;
use Interfaces.C.Strings;

package body GLUS is


  -- utils
  --

  function Value_Without_Exception(S : chars_ptr) return String is
  -- Translate S from a C-style char* into an Ada String.
  -- If S is Null_Ptr, return "", don't raise an exception.
  begin
    if S = Null_Ptr then
      return "";
    else return
      Value(S);
    end if;
  end Value_Without_Exception;

  pragma Inline(Value_Without_Exception);

  function ToString(ptr : GLubyte_Ptr) return String is
  begin
    return Value_Without_Exception(chars_ptr(ptr));
  end ToString;


end GLUS;

