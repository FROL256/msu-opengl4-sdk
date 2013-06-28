#pragma once

#include <GL/glew.h>

#include "GL/glus.h"
#include "../vsgl3/glHelper.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>

class Water
{

public:

  Water();
  virtual ~Water();

  virtual void SimStep();
  virtual void Draw();
  virtual void AddWave(float x, float y);

  GLuint GetRenderProgram() { return m_renderProg.program; }

protected:

  Water(const Water& rhs) {}
  Water& operator=(const Water& rhs) {return *this;}

  ShaderProgram m_renderProg;
  ShaderProgram m_simProgram;

  int m_gridSizeX;
  int m_gridSizeY;

  RenderableTexture2D* m_posTex[3];
  int m_A, m_B, m_C;

  FullScreenQuad*      m_pFullScreenQuad;

  // water mesh
  SimpleMesh*   m_pWaterMesh;  
};



