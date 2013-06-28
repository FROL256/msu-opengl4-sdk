#include "SparksParticleSystem.h"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
SparksParticleSystem::SparksParticleSystem(int a_particlesNumber)
{
  for(int i=0;i<2;i++)
  {
    m_posAndSizeBuffers[i] = -1;
    m_velAndHPBuffers[i]   = -1;
    m_drawVAOs[i]          = -1;
  }

  m_particlesNum = 0;
  m_currPinPongId = 0;

  m_pFogTexture = NULL;

  glGenBuffers(2, m_posAndSizeBuffers); CHECK_GL_ERRORS;
  glGenBuffers(2, m_velAndHPBuffers);   CHECK_GL_ERRORS;
  glGenBuffers(2, m_randBuffers);       CHECK_GL_ERRORS;

  SetParticlesNum(a_particlesNumber);
  Init(); // dispatching call

  m_renderProgram  = ShaderProgram("../ParticleSystem/SparkParticle.vert", "../ParticleSystem/SparkParticle.geom", "../ParticleSystem/SparkParticle.frag");
  m_animateProgram = ShaderProgram("../ParticleSystem/SparkParticlePhysics.vert");

  //m_renderProgram  = ShaderProgram("../ParticleSystem/Particle.vert", "../ParticleSystem/Particle.geom", "../ParticleSystem/FireParticle.frag");
  //m_animateProgram = ShaderProgram("../ParticleSystem/ParticlePhysics.vert");

  const GLchar* names[3] = {"newPosAndSize","newVelAndHp","outRndSeed"};
  glTransformFeedbackVaryings (m_animateProgram.program, 3, names, GL_SEPARATE_ATTRIBS); CHECK_GL_ERRORS;
  if(!m_animateProgram.Link())
    throw std::runtime_error("can not relink program after glTransformFeedbackVaryings");

  CreateBuffers(m_renderProgram.program, m_animateProgram.program);

  m_pFogTexture     = NULL;
  m_pFullScreenQuad = new FullScreenQuad();
  m_lastDeltaTime   = 0.0f;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void SparksParticleSystem::Draw()
{
  glUseProgram(m_renderProgram.program); CHECK_GL_ERRORS;

  setUniform(m_renderProgram.program, "modelViewMatrix",  float4x4(m_modelViewMatrix));
  setUniform(m_renderProgram.program, "projectionMatrix", float4x4(m_projectionMatrix));  
  setUniform(m_renderProgram.program, "delta_t", m_lastDeltaTime);

 // bindTexture(m_renderProgram.program, 1, "colorTex", m_pFogTexture->GetColorTexId());

  glBindVertexArray(m_drawVAOs[1-m_currPinPongId]); CHECK_GL_ERRORS;
  glDrawArrays(GL_POINTS, 0, m_particlesNum);       CHECK_GL_ERRORS; 

}

