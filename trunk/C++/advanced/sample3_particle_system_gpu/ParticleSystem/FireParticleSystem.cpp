#include "FireParticleSystem.h"


//////////////////////////////////////////////////////////////////////////////////////////////////////////
////
FireParticleSystem::FireParticleSystem(int a_particlesNum)
{
  for(int i=0;i<2;i++)
  {
    m_posAndSizeBuffers[i] = -1;
    m_velAndHPBuffers[i]   = -1;
    m_drawVAOs[i]          = -1;
  }
  m_vertexPosTBO = -1;

  m_particlesNum = 0;
  m_currPinPongId = 0;

  m_pFogTexture = NULL;

  glGenBuffers(2, m_posAndSizeBuffers); CHECK_GL_ERRORS;
  glGenBuffers(2, m_velAndHPBuffers);   CHECK_GL_ERRORS;
  glGenBuffers(2, m_randBuffers);       CHECK_GL_ERRORS;

  glGenTextures(1, &m_vertexPosTBO);

  SetParticlesNum(a_particlesNum);
  Init(); // dispatching call

  m_renderProgram  = ShaderProgram("../ParticleSystem/Particle.vert", "../ParticleSystem/FireParticle.geom", "../ParticleSystem/FireParticle.frag");
  m_animateProgram = ShaderProgram("../ParticleSystem/FireParticlePhysics.vert");

  const GLchar* names[3] = {"newPosAndSize", "newVelAndHp", "outRndSeed"};
  glTransformFeedbackVaryings (m_animateProgram.program, 3, names, GL_SEPARATE_ATTRIBS); CHECK_GL_ERRORS;
  if(!m_animateProgram.Link())
    throw std::runtime_error("can not relink program after glTransformFeedbackVaryings");

  CreateBuffers(m_renderProgram.program, m_animateProgram.program);

  //m_pFogTexture = new Texture2D("../data/fog.bmp");
  //m_pFogTexture = new Texture2D("../data/particle.tga");
  m_pFogTexture = new Texture2D("../data/fire_texturemap_small_grey.bmp");

  m_pFullScreenQuad = new FullScreenQuad();
  m_lastDeltaTime = 0.0f;
  m_windChangeTime = 0.0f;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////
////
FireParticleSystem::~FireParticleSystem()
{
  glDeleteTextures(1, &m_vertexPosTBO);
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void FireParticleSystem::UpdateParameters(float delta_t)
{
  m_windChangeTime += delta_t;
  if(m_windChangeTime > 2.0f)
  {
    m_wind.x = rnd(-0.5f, 0.5f);
    m_wind.z = rnd(-0.5f, 0.5f);
    m_windChangeTime = 0.0f;
  }

}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void FireParticleSystem::Process(float delta_t)
{
  UpdateParameters(delta_t);

  glUseProgram(m_animateProgram.program); CHECK_GL_ERRORS;
  setUniform(m_animateProgram.program, "delta_t", delta_t);
  setUniform(m_animateProgram.program, "wind", m_wind);
  //setUniform(m_animateProgram.program, "epicenter", float3(0,1,0));

  glBindBuffer(GL_TEXTURE_BUFFER, m_posAndSizeBuffers[1-m_currPinPongId]);          CHECK_GL_ERRORS;
  bindTextureBuffer(m_animateProgram.program, 1, "vertPosBuffer", m_vertexPosTBO);  CHECK_GL_ERRORS;
  glTexBuffer(GL_TEXTURE_BUFFER, GL_RGBA32F, m_vertexPosTBO);                       CHECK_GL_ERRORS;

  glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, m_posAndSizeBuffers[m_currPinPongId]); CHECK_GL_ERRORS;
  glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 1, m_velAndHPBuffers[m_currPinPongId]);   CHECK_GL_ERRORS;
  glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 2, m_randBuffers[m_currPinPongId]);       CHECK_GL_ERRORS;

  glEnable(GL_RASTERIZER_DISCARD);
  glBeginTransformFeedback(GL_POINTS); CHECK_GL_ERRORS;

  glBindVertexArray(m_animVAOs[1-m_currPinPongId]); CHECK_GL_ERRORS;
  glDrawArrays(GL_POINTS, 0, m_particlesNum);       CHECK_GL_ERRORS;  

  glBindVertexArray(0); 

  glEndTransformFeedback(); CHECK_GL_ERRORS;
  glDisable(GL_RASTERIZER_DISCARD);

  glBindBuffer(GL_TEXTURE_BUFFER, 0);
  glBindTexture(GL_TEXTURE_BUFFER, 0);

  m_currPinPongId = 1 - m_currPinPongId;
  m_lastDeltaTime = delta_t;
}

