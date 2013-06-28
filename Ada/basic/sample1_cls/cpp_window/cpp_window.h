/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#ifndef CPP_WINDOW_GUARDIAN
#define CPP_WINDOW_GUARDIAN

#include <GL/glew.h>
#include "GL/glus.h"

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

// from cpp to ada
//
extern "C" void cpp_create_context_and_window(int ver, int subver, int width, int height);
extern "C" void cpp_glew_init();
extern "C" void cpp_main_loop();

// from ada to cpp
//
extern "C" GLUSboolean ada_render_frame(float a_deltaTime);
extern "C" GLUSboolean ada_on_create_window();
extern "C" void        ada_on_destroy_window();
extern "C" void        ada_on_reshape_window(int width, int height);
extern "C" void        ada_keyboard(GLUSboolean pressed, GLUSuint key);
extern "C" void        ada_mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y);
extern "C" void        ada_mouse_move(GLUSuint button, GLUSint x, GLUSint y);


#endif

