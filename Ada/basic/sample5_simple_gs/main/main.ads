with Interfaces;
with Interfaces.C;
with Ada.Text_IO;

use Interfaces;
use Ada.Text_IO;

package main is

  procedure Main;

  type Input is record
    mx : float := 0.0;
    my : float := 0.0;

    ldown : boolean := false;
    rdown : boolean := false;

    cam_rot_x : float;
    cam_rot_y : float;
    cam_rot_z : float;

    cam_pos_z : float := -2.0;

  end record;

  -- export this functions to cpp
  --
  function RenderFrame(a_deltaTime : float) return Interfaces.C.unsigned_char;
  pragma Export(CPP, RenderFrame, "ada_render_frame");

  function InitWindow return Interfaces.C.unsigned_char;
  pragma Export(CPP, InitWindow, "ada_on_create_window");

  procedure DestroyWindow;
  pragma Export(CPP, DestroyWindow, "ada_on_destroy_window");

  procedure Reshape(width : Integer; height : Integer);
  pragma Export(CPP, Reshape, "ada_on_reshape_window");

  procedure Keyboard(pressed : Interfaces.C.unsigned_char; key : Integer);
  pragma Export(CPP, Keyboard, "ada_keyboard");

  procedure Mouse(pressed : Interfaces.C.unsigned_char; button : Integer; x : Integer; y : Integer);
  pragma Export(CPP, Mouse, "ada_mouse");

  procedure MouseMove(button : Integer; x : Integer; y : Integer);
  pragma Export(CPP, MouseMove, "ada_mouse_move");

end main;
