with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Sequential_IO;
with Ada.Real_Time;
with Interfaces;
with Interfaces.C;

with GLUS;
with GL_H;

use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Interfaces;
use Ada.Real_Time;
use Interfaces.C;
use GLUS;
use GL_H;

package body main is

  procedure Print_System_Info is

    vendor   : GLubyte_Ptr := glGetString(GL_VENDOR);
    gpuName  : GLubyte_Ptr := glGetString(GL_RENDERER);
    gl_ver   : GLubyte_Ptr := glGetString(GL_VERSION);
    glsl_ver : GLubyte_Ptr := glGetString(GL_SHADING_LANGUAGE_VERSION);

  begin

    Put("GPU Vendor: "); Put_Line(ToString(vendor));
    Put("GPU Name  : "); Put_Line(ToString(gpuName));
    Put("GL Ver    : "); Put_Line(ToString(gl_ver));
    Put("GLSL Ver  : "); Put_Line(ToString(glsl_ver));

  end Print_System_Info;


  --
  --
  function On_Create_Window return GLUSboolean is
  begin
    return GLUS_TRUE;
  end On_Create_Window;


  procedure On_Destroy_Window is
  begin
    null;
  end On_Destroy_Window;



  procedure Keyboard(pressed : GLUSboolean; key : Integer) is
  begin
    null;
  end Keyboard;

  procedure Mouse(pressed : GLUSboolean; button : Integer; x : Integer; y : Integer) is
  begin

    if integer(pressed) = 0 then
      return;
    end if;

    if (Unsigned_32(button) and Unsigned_32(1)) > 0 then
      Put_Line("left button pressed");
    end if;

    if (Unsigned_32(button) and Unsigned_32(4)) > 0 then
      Put_Line("right button pressed");
    end if;


  end Mouse;


  procedure Mouse_Move(button : Integer; x : Integer; y : Integer) is

  begin

    null;

  end Mouse_Move;



  --
  --
  procedure Reshape(width : Integer; height : Integer) is

  begin

    glViewport(0, 0, width, height);

  end Reshape;

  --
  --
  function Render_Frame(a_deltaTime : float) return GLUSboolean is
  begin

    glClearColor(0.0, 0.0, 1.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);


    return GLUS_TRUE;

  end Render_Frame;


  -- entry point
  --
  procedure Main is

  begin

    Put_Line("start program");

    cpp_create_context_and_window(3,0,640,480);

    cpp_glew_init;

    Print_System_Info;

    cpp_main_loop;

    Put_Line("end program");

  end Main;


end main;
