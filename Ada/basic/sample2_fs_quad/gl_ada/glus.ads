with Interfaces;
with Interfaces.C;
with Ada.Text_IO;
with GL_H;

use Interfaces;
use Ada.Text_IO;
use GL_H;

package GLUS is

  procedure cpp_create_context_and_window(ver : Integer; subver : Integer; width : Integer; height : Integer);
  pragma Import(C, cpp_create_context_and_window, "cpp_create_context_and_window");


  procedure cpp_main_loop;  pragma Import(C, cpp_main_loop, "cpp_main_loop");
  procedure cpp_glew_init;  pragma Import(C, cpp_glew_init, "cpp_glew_init");


  function ToString(ptr : GLubyte_Ptr) return String;

  subtype GLUSboolean is Interfaces.C.unsigned_char;
  subtype GLUSuint    is Interfaces.C.unsigned;

  GLUS_TRUE  : constant GLUSboolean := 1;
  GLUS_FALSE : constant GLUSboolean := 0;

end GLUS;


