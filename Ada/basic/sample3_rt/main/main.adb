with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Sequential_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Interfaces;
with Interfaces.C;

with GLUS;
with GL_H;
with GL_EXT;
with GL_HELPERS;
with Vector_Math;

use Ada.Text_IO;
use Ada.Integer_Text_IO;
use Interfaces;
use Interfaces.C;

use GLUS;
use GL_H;
use GL_EXT;
use GL_HELPERS;
use Vector_Math;

package body main is

  package Float_Functions is new Ada.Numerics.Generic_Elementary_Functions (float);
  use Float_Functions;

  g_prog  : ShaderProgram;
  g_quad  : FullScreenQuadRef;
  g_input : Input;

  g_screenWidth  : Integer;
  g_screenHeight : Integer;

  g_matAmbientColor  : float3;
  g_matDiffuseColor  : float3;
  g_matSpecularColor : float3;

  g_elaspedTimeFromStart : float := 0.0;

  procedure PrintSystemInfo is

    vendor   : GLubyte_Ptr := glGetString(GL_VENDOR);
    gpuName  : GLubyte_Ptr := glGetString(GL_RENDERER);
    gl_ver   : GLubyte_Ptr := glGetString(GL_VERSION);
    glsl_ver : GLubyte_Ptr := glGetString(GL_SHADING_LANGUAGE_VERSION);

  begin

    Put("GPU Vendor: "); Put_Line(ToString(vendor));
    Put("GPU Name  : "); Put_Line(ToString(gpuName));
    Put("GL Ver    : "); Put_Line(ToString(gl_ver));
    Put("GLSL Ver  : "); Put_Line(ToString(glsl_ver));

  end PrintSystemInfo;


  --
  --
  function InitWindow return GLUSboolean is
  begin

    g_prog := CreateShaderProgram("../main/Vertex.vert", "../main/Fragment.frag");
    g_quad := new FullScreenQuad;

    g_matAmbientColor  := (0.2, 0.2, 0.2);
    g_matDiffuseColor  := (0.25, 0.25, 0.25);
    g_matSpecularColor := (0.5, 0.5, 0.5);


    return GLUS_TRUE;
  end InitWindow;


  procedure DestroyWindow is
  begin

    DestroyShaderProgram(g_prog);

    -- C++ guys, please note that we do not have to call 'Finalize(g_quad)' {or Free or whatever} manually (but fo sure we can, like we did with g_prog)
    -- due to FullScreenQuad is 'limited', Ada will automaticly call destructor when reference leaves it's scope
    -- Free(g_quad);


  end DestroyWindow;



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
      g_input.ldown := true;
      g_input.mx := float(x);
      g_input.my := float(y);
    end if;

    if (Unsigned_32(button) and Unsigned_32(4)) > 0 then
      g_input.rdown := true;
      g_input.mx := float(x);
      g_input.my := float(y);
    end if;


  end Mouse;


  procedure MouseMove(button : Integer; x : Integer; y : Integer) is
    x1,y1 : float;
  begin

    if((Unsigned_32(button) and Unsigned_32(1)) > 0) then -- left button

      x1 := float(x);
      y1 := float(y);

      g_input.cam_rot_x := g_input.cam_rot_x + 0.25*(y1 - float(g_input.my));
      g_input.cam_rot_y := g_input.cam_rot_y + 0.25*(x1 - float(g_input.mx));

      g_input.mx := float(x);
      g_input.my := float(y);

    end if;

  end MouseMove;



  --
  --
  procedure Reshape(width : Integer; height : Integer) is

  begin

    g_screenWidth  := width;
    g_screenHeight := height;

    glViewport(0, 0, width, height);


  end Reshape;

  --
  --
  function RenderFrame(a_deltaTime : float) return GLUSboolean is
    modelRotMatrixX  : float4x4;
    modelRotMatrixY  : float4x4;
    modelTransMatrix : float4x4;
    rayMatrix      : float4x4;

    pointLightColor : float3 := (0.95, 0.95, 0.95);
    environmentLightColor : float3 := (1.5, 1.5, 1.5);

    lightPos : float3;
  begin

    g_elaspedTimeFromStart := g_elaspedTimeFromStart + a_deltaTime;

    lightPos.x := 5.0*sin(1.0*g_elaspedTimeFromStart);
    lightPos.y := 5.0*cos(1.0*g_elaspedTimeFromStart);
    lightPos.z := 5.0*sin(1.0*g_elaspedTimeFromStart);

    glClearColor(0.0, 0.0, 1.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);

    glUseProgram(g_prog.program);

    modelRotMatrixX  := RotationMatrix(-0.025*g_input.cam_rot_x, (1.0,0.0,0.0));
    modelRotMatrixY  := RotationMatrix(-0.025*g_input.cam_rot_y, (0.0,1.0,0.0));
    modelTransMatrix := TranslationMatrix((0.0,0.0,4.0));

    -- because in this sample all transformations are inversed because they applied to rays, not to objects
    --
    rayMatrix      := modelRotMatrixY*modelRotMatrixX*modelTransMatrix;


    setUniform(g_prog.program, "g_pointLightColor", pointLightColor);
    setUniform(g_prog.program, "g_environmentLightColor", environmentLightColor);

    setUniform(g_prog.program, "g_rayMatrix",    rayMatrix);
    setUniform(g_prog.program, "g_screenWidth",  g_screenWidth);
    setUniform(g_prog.program, "g_screenHeight", g_screenHeight);

    setUniform(g_prog.program, "g_matAmbientColor", g_matAmbientColor);
    setUniform(g_prog.program, "g_matDiffuseColor", g_matDiffuseColor);
    setUniform(g_prog.program, "g_matSpecularColor", g_matSpecularColor);

    setUniform(g_prog.program, "g_lightPos",  lightPos);


    Draw(g_quad);

    return GLUS_TRUE;

  end RenderFrame;


  -- entry point
  --
  procedure Main is

  begin

    Put_Line("start program");

    cpp_create_context_and_window(3,0,640,480);
    cpp_glew_init;

    PrintSystemInfo;

    cpp_main_loop;

    Put_Line("end program");

  end Main;


end main;
