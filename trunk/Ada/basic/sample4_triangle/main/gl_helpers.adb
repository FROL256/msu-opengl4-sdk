with Interfaces;
with Interfaces.C;
with Ada.Text_IO;
with GL_H;
with GL_EXT;

use Interfaces;
use Interfaces.C;
use Ada.Text_IO;
use GL_H;
use GL_EXT;

package body GL_HELPERS is


  function CreateShaderProgram(vs_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(""), To_C(""), To_C(""), To_C(""));
  end CreateShaderProgram;

  function CreateShaderProgram(vs_path : String; ps_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(""), To_C(""), To_C(""), To_C(ps_path));
  end CreateShaderProgram;

  function CreateShaderProgram(vs_path : String; gs_path : String; ps_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(""), To_C(""), To_C(gs_path), To_C(ps_path));
  end CreateShaderProgram;

  function CreateShaderProgram(vs_path : String; ts_path : String; es_path : String; gs_path : String; ps_path : String) return ShaderProgram is
  begin
    return cpp_CreateShaderProgram(To_C(vs_path), To_C(ts_path), To_C(es_path), To_C(gs_path), To_C(ps_path));
  end CreateShaderProgram;



   procedure DestroyShaderProgram(a_prog : in out ShaderProgram) is

   begin

     if a_prog.program > 0 then

      glDeleteProgram(a_prog.program);

      if a_prog.vertex > 0 then
        glDeleteShader(a_prog.vertex);
      end if;

      if a_prog.control > 0 then
        glDeleteShader(a_prog.control);
      end if;

      if a_prog.evaluation > 0 then
        glDeleteShader(a_prog.evaluation);
      end if;

      if a_prog.geometry > 0 then
        glDeleteShader(a_prog.geometry);
      end if;

      if a_prog.fragment > 0 then
        glDeleteShader(a_prog.fragment);
      end if;

     end if;


  end DestroyShaderProgram;

  procedure Initialize(self : in out FullScreenQuad) is
    vertexLocation : GLuint := 0;
    quadPos : array (1 .. 8) of float;
  begin
    Put_Line("FullScreenQuad Init");
    self.vbo := 0;
    self.vao := 0;
    quadPos  := (-1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 1.0, -1.0);

    self.vbo := glGenBuffer;
    glBindBuffer(GL_ARRAY_BUFFER, self.vbo);                                    CHECK_GL_ERRORS;
    glBufferData(GL_ARRAY_BUFFER, GL_EXT_Float_Array(quadPos), GL_STATIC_DRAW); CHECK_GL_ERRORS;

    self.vao := glGenVertexArray;
    glBindVertexArray(self.vao);

    glBindBuffer(GL_ARRAY_BUFFER, self.vbo);                                    CHECK_GL_ERRORS;
    glEnableVertexAttribArray(vertexLocation);                                  CHECK_GL_ERRORS;
    glVertexAttribPointer(vertexLocation, 2, GL_FLOAT, GLboolean(GL_FALSE), 0); CHECK_GL_ERRORS;

  end Initialize;


  procedure Finalize(self : in out FullScreenQuad) is
  begin
    Put_Line("FullScreenQuad Destoy");

    if self.vbo > 0 then
      glDeleteBuffer(self.vbo);
    end if;

    if self.vao > 0 then
      glDeleteVertexArray(self.vao);
    end if;

  end Finalize;

  procedure Draw(quad : FullScreenQuadRef) is
  begin

    glBindVertexArray(quad.vao);            CHECK_GL_ERRORS;
    glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);  CHECK_GL_ERRORS;

  end Draw;



  procedure Initialize(self : in out GeometryShape) is
  begin
    null;
  end Initialize;


  procedure Finalize(self : in out GeometryShape) is

  begin
     Put_Line("GeometryShape Destoyed");

    if self.vbo_pos > 0 then
      glDeleteBuffer(self.vbo_pos);
    end if;

    if self.vbo_norm > 0 then
      glDeleteBuffer(self.vbo_norm);
    end if;

    if self.vbo_tang > 0 then
      glDeleteBuffer(self.vbo_tang);
    end if;

    if self.vao > 0 then
      glDeleteVertexArray(self.vao);
    end if;

  end Finalize;


  -- returning ref is ok because GeometryShape is controlled type (i.e. Ada runtime will automaticly release memory)
  --
  function CreateShapePlane(sizeX : float; sizeY : float; subdivsX : integer; subdivsY : integer) return GeometryShapeRef is
    shape : GeometryShapeRef := null;
    vertexLocation : GLuint := 0; -- simple layout, assume have only positions at location = 0
    positions : array(0..subdivsX*subdivsY*24) of float;
  begin

    shape := new GeometryShape;
    shape.vert_num := subdivsX*subdivsY*6;

    -- you may create indexed layout if you want but i'm too lazy for that
    --
    declare
      minX : float := -0.5*sizeX;
      minY : float := -0.5*sizeY;

      maxX : float := +0.5*sizeX;
      maxY : float := +0.5*sizeY;

      v : array(0..3) of float3;
      vertPos  : integer := 0;
      tx0,ty0,tx1,ty1  : float;

    begin

      --
      for x in 0..subdivsX-1 loop

        tx0 := float(x)/float(subdivsX);
        tx1 := float(x+1)/float(subdivsX);

        for y in 0..subdivsY-1 loop

          ty0 := float(y)/float(subdivsY);
          ty1 := float(y+1)/float(subdivsY);

          v(0).x := minX + tx0*(maxX-minX);
          v(0).y := 0.0;
          v(0).z := minY + ty1*(maxY-minY);

          v(1).x := minX + tx0*(maxX-minX);
          v(1).y := 0.0;
          v(1).z := minY + ty0*(maxY-minY);

          v(2).x := minX + tx1*(maxX-minX);
          v(2).y := 0.0;
          v(2).z := minY + ty1*(maxY-minY);

          v(3).x := minX + tx1*(maxX-minX);
          v(3).y := 0.0;
          v(3).z := minY + ty0*(maxY-minY);

          -- push triangle 1
          --
          positions(vertPos + 0) := v(0).x;
          positions(vertPos + 1) := v(0).y;
          positions(vertPos + 2) := v(0).z;
          positions(vertPos + 3) := 1.0;

          positions(vertPos + 4) := v(1).x;
          positions(vertPos + 5) := v(1).y;
          positions(vertPos + 6) := v(1).z;
          positions(vertPos + 7) := 1.0;

          positions(vertPos + 8)  := v(2).x;
          positions(vertPos + 9)  := v(2).y;
          positions(vertPos + 10) := v(2).z;
          positions(vertPos + 11) := 1.0;

          -- push triangle 2
          --
          positions(vertPos + 12) := v(2).x;
          positions(vertPos + 13) := v(2).y;
          positions(vertPos + 14) := v(2).z;
          positions(vertPos + 15) := 1.0;

          positions(vertPos + 16) := v(1).x;
          positions(vertPos + 17) := v(1).y;
          positions(vertPos + 18) := v(1).z;
          positions(vertPos + 19) := 1.0;

          positions(vertPos + 20) := v(3).x;
          positions(vertPos + 21) := v(3).y;
          positions(vertPos + 22) := v(3).z;
          positions(vertPos + 23) := 1.0;

          vertPos := vertPos + 4*6;

        end loop;

      end loop;

      shape.vert_num := vertPos/4;

    end;


    --Put("shape.vert_num = "); Put_line(integer'Image(shape.vert_num));

    shape.vbo_pos := glGenBuffer;                                                  CHECK_GL_ERRORS;
    glBindBuffer(GL_ARRAY_BUFFER, shape.vbo_pos);                                  CHECK_GL_ERRORS;
    glBufferData(GL_ARRAY_BUFFER, GL_EXT_Float_Array(positions), GL_STATIC_DRAW);  CHECK_GL_ERRORS;

    shape.vao := glGenVertexArray;                                                 CHECK_GL_ERRORS;
    glBindVertexArray(shape.vao);                                                  CHECK_GL_ERRORS;

    glBindBuffer(GL_ARRAY_BUFFER, shape.vbo_pos);                                  CHECK_GL_ERRORS;
    glEnableVertexAttribArray(vertexLocation);                                     CHECK_GL_ERRORS;
    glVertexAttribPointer(vertexLocation, 4, GL_FLOAT, GLboolean(GL_FALSE), 0);    CHECK_GL_ERRORS;

    glBindVertexArray(0);

    return shape;

  end CreateShapePlane;

  procedure Draw(shape : GeometryShapeRef) is
  begin
    glBindVertexArray(shape.vao); CHECK_GL_ERRORS;
    glDrawArrays(GL_TRIANGLES, 0, shape.vert_num);   CHECK_GL_ERRORS;
  end Draw;


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : integer) is
    loc : GLint := glGetUniformLocation(a_prog, a_name);
  begin
    if loc >= 0 then
      glUniform1i(loc, a_val);
    end if;
  end setUniform;

  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float) is
    loc : GLint := glGetUniformLocation(a_prog, a_name);
  begin
    if loc >= 0 then
      glUniform1f(loc, a_val);
    end if;
  end setUniform;


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float4x4) is
   loc : GLint    := glGetUniformLocation(a_prog, a_name);
   mt  : float4x4 := transpose(a_val);
  begin
    if loc >= 0 then
      glUniformMatrix4fv(loc, 1, GLBoolean(GL_FALSE), mt(0,0)'address);
    end if;
  end setUniform;


  procedure setUniform(a_prog : GLuint; a_name : String; a_val : float3) is
    loc : GLint := glGetUniformLocation(a_prog, a_name);
  begin
     if loc >= 0 then
      glUniform3f(loc, a_val.x, a_val.y, a_val.z);
    end if;
  end setUniform;




  function ProjectionMatrix(fovy : float; aspect : float; zNear : float; zFar : float) return float4x4 is
    res : float4x4;
  begin
    glusPerspectivef(res(0,0)'Address, fovy, aspect, zNear, zFar);
    return transpose(res);
  end ProjectionMatrix;


  function LookAtMatrixGLUS(eye : float3; center : float3; upVector : float3 ) return float4x4 is
    res : float4x4;
  begin
    glusLookAtf(res(0,0)'Address, eye.x, eye.y, eye.z, center.x, center.y, center.z, upVector.x, upVector.y, upVector.z);
    return transpose(res);
  end LookAtMatrixGLUS;


  procedure PrintMatrix(mat : float4x4) is
  begin
    for i in 0..3 loop
      for j in 0..3 loop
        Put(float'image(mat(i,j)));
        Put(" ");
      end loop;
      Put_Line("");
    end loop;
  end PrintMatrix;



end GL_HELPERS;

