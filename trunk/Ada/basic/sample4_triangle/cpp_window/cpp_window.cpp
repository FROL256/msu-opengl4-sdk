/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "cpp_window.h"

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

void* cpp_nullPointer = NULL;

GLUSboolean init(GLUSvoid)   { ada_on_create_window(); }
GLUSvoid terminate(GLUSvoid) { ada_on_destroy_window(); }
GLUSvoid reshape(GLUSuint width, GLUSuint height) { ada_on_reshape_window(width, height); }
GLUSvoid mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y) { ada_mouse(pressed, button, x, y); }
GLUSvoid mouseMove(GLUSuint button, GLUSint x, GLUSint y) { ada_mouse_move(button,x,y);}
GLUSvoid keyboard(GLUSboolean pressed, GLUSuint key) { ada_keyboard(pressed, key); }
GLUSboolean update(GLUSfloat a_deltaTime) { return ada_render_frame(a_deltaTime); }


extern "C" void cpp_glew_init()
{
  glewExperimental = GL_TRUE;

  GLenum err=glewInit();
  if(err!=GLEW_OK)
  {
    printf("glewInitError: %s\n", glewGetErrorString(err));
    return;
  }

  glGetError(); // flush error state variable, caused by glew errors
}


extern "C" void cpp_create_context_and_window(int ver, int subver, int width, int height)
{
  glusInitFunc(init);
  glusReshapeFunc(reshape);
  glusUpdateFunc(update);
  glusTerminateFunc(terminate);
  glusMouseFunc(mouse);
  glusMouseMoveFunc(mouseMove);
  glusKeyFunc(keyboard);

  glusPrepareContext(ver, subver, GLUS_FORWARD_COMPATIBLE_BIT);

  if (!glusCreateWindow("Mixed Ada/C++ OpenGL Sample", width, height, GLUS_FALSE))
  {
      printf("Could not create window!");
      return;
  }

}

extern "C" void cpp_main_loop() { glusRun(); }

