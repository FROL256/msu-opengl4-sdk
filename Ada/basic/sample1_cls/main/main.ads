with Interfaces;
with Interfaces.C;
with Ada.Text_IO;

use Interfaces;
use Ada.Text_IO;

package main is

  procedure Main;

  -- export this functions to cpp
  --
  function Render_Frame(a_deltaTime : float) return Interfaces.C.unsigned_char;
  pragma Export(CPP, Render_Frame, "ada_render_frame");

  function On_Create_Window return Interfaces.C.unsigned_char;
  pragma Export(CPP, On_Create_Window, "ada_on_create_window");

  procedure On_Destroy_Window;
  pragma Export(CPP, On_Destroy_Window, "ada_on_destroy_window");

  procedure Reshape(width : Integer; height : Integer);
  pragma Export(CPP, Reshape, "ada_on_reshape_window");

  procedure Keyboard(pressed : Interfaces.C.unsigned_char; key : Integer);
  pragma Export(CPP, Keyboard, "ada_keyboard");

  procedure Mouse(pressed : Interfaces.C.unsigned_char; button : Integer; x : Integer; y : Integer);
  pragma Export(CPP, Mouse, "ada_mouse");

  procedure Mouse_Move(button : Integer; x : Integer; y : Integer);
  pragma Export(CPP, Mouse_Move, "ada_mouse_move");

end main;
