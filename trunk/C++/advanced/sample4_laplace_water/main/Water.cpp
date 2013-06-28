#include "Water.h"


Water::Water()
{
  m_renderProg = ShaderProgram("../main/Water.vert", "../main/Water.frag");
  m_simProgram = ShaderProgram("../main/Quad.vert", "../main/WaterSimStep.frag");
  
  m_pWaterMesh = new SimpleMesh(m_renderProg.program, 256, SimpleMesh::PLANE, 0.999999f);
  
  m_pFullScreenQuad = new FullScreenQuad();

  m_gridSizeX = 256;
  m_gridSizeY = 256;

  for(int i=0;i<3;i++)
    m_posTex[i] = new RenderableTexture2D(GL_R32F, m_gridSizeX, m_gridSizeY);

  m_A = 0; 
  m_B = 1; 
  m_C = 2;

  // init positions
  //
  GLint oldViewport[4];
  glGetIntegerv(GL_VIEWPORT, oldViewport);
  glViewport(0, 0, m_gridSizeX, m_gridSizeX);
  
  glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
  for(int i=0;i<3;i++)
  {
    m_posTex[i]->BeginRenderingToThisTexture();
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);
    m_posTex[i]->EndRenderingToThisTexture();
  }

  glViewport(oldViewport[0], oldViewport[1], oldViewport[2], oldViewport[3]);
}

Water::~Water()
{
  delete m_pWaterMesh; m_pWaterMesh = NULL;
  delete m_pFullScreenQuad; m_pFullScreenQuad = NULL;

  for(int i=0;i<3;i++)
  {
    delete m_posTex[i];
    m_posTex[i] = NULL;
  }

}

void Water::AddWave(float x, float y)
{
  if (x < 0.0) x = 0.0;
  if (y < 0.0) y = 0.0;

  if (x > 1.0) x = 1.0;
  if (y > 1.0) y = 1.0;

  int waveSizeX = 3;
  int waveSizeY = 3;

  int pixelPosX = int( x*m_gridSizeX - 0.5f*float(waveSizeX) + 0.5f);
  int pixelPosY = int( y*m_gridSizeY - 0.5f*float(waveSizeY) + 0.5f);

  if(pixelPosX < 0) pixelPosX = 0;
  if(pixelPosY < 0) pixelPosY = 0;

  if(pixelPosX >= m_gridSizeX-waveSizeX) pixelPosX = m_gridSizeX-waveSizeX;
  if(pixelPosY >= m_gridSizeY-waveSizeY) pixelPosY = m_gridSizeY-waveSizeY;

  float* cpuData = new float[waveSizeX*waveSizeY];

  for(int i=0;i<waveSizeX*waveSizeY;i++)
    cpuData[i] = -0.05f;

  glBindTexture(GL_TEXTURE_2D, m_posTex[0]->GetColorTexId());
  glTexSubImage2D(GL_TEXTURE_2D,0,pixelPosX,pixelPosY,waveSizeX,waveSizeY,GL_RED,GL_FLOAT,cpuData);
  glGenerateMipmap(GL_TEXTURE_2D);

  glBindTexture(GL_TEXTURE_2D, m_posTex[1]->GetColorTexId());
  glTexSubImage2D(GL_TEXTURE_2D,0,pixelPosX,pixelPosY,waveSizeX,waveSizeY,GL_RED,GL_FLOAT,cpuData);
  glGenerateMipmap(GL_TEXTURE_2D);

  delete [] cpuData;

}

void Water::SimStep()
{
  GLint oldViewport[4];
  glGetIntegerv(GL_VIEWPORT, oldViewport);

  glViewport(0, 0, m_gridSizeX, m_gridSizeX);

  glDisable(GL_DEPTH_TEST);

  int temp = m_A;
  m_A = m_B;
  m_B = m_C;
  m_C = temp;
 

  // pos step
  //
  m_posTex[m_C]->BeginRenderingToThisTexture();
  
  glUseProgram(m_simProgram.program);

  setUniform(m_simProgram.program, "gridSizeX", m_gridSizeX);
  setUniform(m_simProgram.program, "gridSizeY", m_gridSizeY);

  bindTexture(m_simProgram.program, 1, "inPositionsPrev",  m_posTex[m_A]->GetColorTexId());
  bindTexture(m_simProgram.program, 2, "inPositions",      m_posTex[m_B]->GetColorTexId());

  m_pFullScreenQuad->Draw();

  m_posTex[m_C]->EndRenderingToThisTexture();

  // restore old viewport and depth test
  //
  glEnable(GL_DEPTH_TEST);
  glViewport(oldViewport[0], oldViewport[1], oldViewport[2], oldViewport[3]);
}


void Water::Draw()
{
  setUniform(m_renderProg.program, "gridSizeX", m_gridSizeX);
  setUniform(m_renderProg.program, "gridSizeY", m_gridSizeY);
  bindTexture(m_renderProg.program, 1, "inPositions", m_posTex[m_C]->GetColorTexId());

  m_pWaterMesh->Draw(GL_TRIANGLES);
}

