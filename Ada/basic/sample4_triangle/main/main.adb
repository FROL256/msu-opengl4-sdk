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
  g_input : Input;

  g_screenWidth  : Integer;
  g_screenHeight : Integer;

  g_vertexBufferObject : GLuint := 0;
  g_vertexArrayObject  : GLuint := 0;

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

    declare
      trianglePos : array(0..5) of float := ( -0.5, -0.5, +0.5, -0.5, +0.0, +0.5 );
      vertexLocation : GLuint := 0; -- simple layout, assume have only positions at location = 0
    begin

      g_vertexBufferObject := glGenBuffer;                                             CHECK_GL_ERRORS;
      glBindBuffer(GL_ARRAY_BUFFER, g_vertexBufferObject);                             CHECK_GL_ERRORS;
      glBufferData(GL_ARRAY_BUFFER, GL_EXT_Float_Array(trianglePos), GL_STATIC_DRAW);  CHECK_GL_ERRORS;

      g_vertexArrayObject := glGenVertexArray;                                      CHECK_GL_ERRORS;
      glBindVertexArray(g_vertexArrayObject);                                       CHECK_GL_ERRORS;

      glBindBuffer(GL_ARRAY_BUFFER, g_vertexBufferObject);                          CHECK_GL_ERRORS;
      glEnableVertexAttribArray(vertexLocation);                                    CHECK_GL_ERRORS;
      glVertexAttribPointer(vertexLocation, 2, GL_FLOAT, GLboolean(GL_FALSE), 0);   CHECK_GL_ERRORS;

      glBindVertexArray(0);

    end;


    return GLUS_TRUE;
  end InitWindow;


  procedure DestroyWindow is
  begin

    DestroyShaderProgram(g_prog);

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
    modelRotX, modelRotY, modelTrans : float4x4;
    projectionMat, modelViewMat   : float4x4;
  begin

    g_elaspedTimeFromStart := g_elaspedTimeFromStart + a_deltaTime;


    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
    glEnable(GL_DEPTH_TEST);

    glUseProgram(g_prog.program);

    modelRotX  := RotationMatrix(-0.025*g_input.cam_rot_x, (1.0,0.0,0.0));
    modelRotY  := RotationMatrix(-0.025*g_input.cam_rot_y, (0.0,1.0,0.0));
    modelTrans := TranslationMatrix((0.0,0.0,0.0));

    modelViewMat   := LookAtMatrixGLUS( (0.0, 0.0, -2.0),
                                        (0.0, 0.0, 0.0),
                                        (0.0, 1.0, 0.0));

    --PrintMatrix(modelViewMat);

    modelViewMat  := modelViewMat*(modelTrans*modelRotX*modelRotY); -- modelViewMat*modelmatrix
    projectionMat := ProjectionMatrix(45.0, float(g_screenWidth)/float(g_screenHeight), 1.0, 100.0);

    setUniform(g_prog.program, "modelViewMatrix",  modelViewMat);
    setUniform(g_prog.program, "projectionMatrix", projectionMat);

    -- draw triangles
    --
    glBindVertexArray(g_vertexArrayObject); CHECK_GL_ERRORS;
    glDrawArrays(GL_TRIANGLES, 0, 3);       CHECK_GL_ERRORS;  -- The last parameter of glDrawArrays is equal to VS invocations

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
