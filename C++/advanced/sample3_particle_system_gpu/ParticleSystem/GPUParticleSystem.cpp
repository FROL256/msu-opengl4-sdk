#include <math.h>
#include "GPUParticleSystem.h"

GPUparticleSystem::GPUparticleSystem()
{
  m_pFogTexture = NULL;
  m_pFullScreenQuad = NULL;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
GPUparticleSystem::GPUparticleSystem(int a_particlesNum)
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
  
  SetParticlesNum(a_particlesNum);
  Init(); // dispatching call

  m_renderProgram  = ShaderProgram("../ParticleSystem/Particle.vert", "../ParticleSystem/Particle.geom", "../ParticleSystem/FireParticle.frag");
  m_animateProgram = ShaderProgram("../ParticleSystem/ParticlePhysics.vert");

  const GLchar* names[3] = {"newPosAndSize","newVelAndHp","outRndSeed"};
  glTransformFeedbackVaryings (m_animateProgram.program, 3, names, GL_SEPARATE_ATTRIBS); CHECK_GL_ERRORS;
  if(!m_animateProgram.Link())
    throw std::runtime_error("can not relink program after glTransformFeedbackVaryings");

  CreateBuffers(m_renderProgram.program, m_animateProgram.program);

  m_pFogTexture = new Texture2D("../data/fog.bmp");
  //m_pFogTexture = new Texture2D("../data/particle.tga");

  m_pFullScreenQuad = new FullScreenQuad();
  m_lastDeltaTime = 0.0f;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::CreateBuffers(GLuint renderProgram, GLuint animateProgram)
{
  // create VAOs for rendering
  //
  int posLocation = glGetAttribLocation(renderProgram, "vertex");
  int velLocation = glGetAttribLocation(renderProgram, "velocity");

  glGenVertexArrays(2, m_drawVAOs);  CHECK_GL_ERRORS;

  for(int i=0;i<2;i++)
  {
    glBindVertexArray(m_drawVAOs[i]);  CHECK_GL_ERRORS;

    if(posLocation >= 0)
    {
      glBindBuffer(GL_ARRAY_BUFFER, m_posAndSizeBuffers[i]);            CHECK_GL_ERRORS;                   
      glEnableVertexAttribArray(posLocation);                           CHECK_GL_ERRORS;
      glVertexAttribPointer(posLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);  CHECK_GL_ERRORS;
    }

    if(velLocation >= 0)
    {
      glBindBuffer(GL_ARRAY_BUFFER, m_velAndHPBuffers[i]); 
      glEnableVertexAttribArray(velLocation);                           CHECK_GL_ERRORS;
      glVertexAttribPointer(velLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);  CHECK_GL_ERRORS;
    }
  }

  // create VAOs for animations
  //
  int posLocation2 = glGetAttribLocation(animateProgram, "vertex");
  int velLocation2 = glGetAttribLocation(animateProgram, "velocity");
  int rndLocation2 = glGetAttribLocation(animateProgram, "inRndSeed");

  glGenVertexArrays(2, m_animVAOs);  CHECK_GL_ERRORS;

  for(int i=0;i<2;i++)
  {
    glBindVertexArray(m_animVAOs[i]);  CHECK_GL_ERRORS;

    if(posLocation2 >=0)
    {
      glBindBuffer(GL_ARRAY_BUFFER, m_posAndSizeBuffers[i]);             CHECK_GL_ERRORS;                   
      glEnableVertexAttribArray(posLocation2);                           CHECK_GL_ERRORS;
      glVertexAttribPointer(posLocation2, 4, GL_FLOAT, GL_FALSE, 0, 0);  CHECK_GL_ERRORS;
    }

    if(velLocation2 >= 0)
    {
      glBindBuffer(GL_ARRAY_BUFFER, m_velAndHPBuffers[i]); 
      glEnableVertexAttribArray(velLocation2);                           CHECK_GL_ERRORS;
      glVertexAttribPointer(velLocation2, 4, GL_FLOAT, GL_FALSE, 0, 0);  CHECK_GL_ERRORS;
    }

    if(rndLocation2 >= 0)
    {
      glBindBuffer(GL_ARRAY_BUFFER, m_randBuffers[i]); 
      glEnableVertexAttribArray(rndLocation2);                                  CHECK_GL_ERRORS;
      glVertexAttribPointer(rndLocation2, 1, GL_FLOAT, GL_FALSE, 0, 0);  CHECK_GL_ERRORS;
    }
  }

  glBindVertexArray(0);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
GPUparticleSystem::~GPUparticleSystem()
{
  Clear();

  if(m_posAndSizeBuffers[0]!=-1 && m_posAndSizeBuffers[1]!=-1)
  {
    glDeleteBuffers(2, m_posAndSizeBuffers);
    glDeleteBuffers(2, m_velAndHPBuffers);
    glDeleteBuffers(2, m_randBuffers);
    glDeleteVertexArrays(2, m_drawVAOs);
    glDeleteVertexArrays(2, m_animVAOs);
  }

  delete m_pFogTexture; m_pFogTexture = NULL;
  delete m_pFullScreenQuad; m_pFullScreenQuad = NULL;
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::Clear()
{
  m_emittersPos.resize(0);
  m_emittersNorm.resize(0);
  m_emittersCos.resize(0);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::Init()
{
  Clear();

  // init position buffers
  //
  std::vector<float4> tempArray(m_particlesNum); 
  
  float size = 1.0f;
  for(int i=0;i<m_particlesNum;i++)
  {
    tempArray[i].x = rnd(-size,size) + 0;
    tempArray[i].y = rnd(-size,size) + 1;
    tempArray[i].z = rnd(-size,size) + 0;
    tempArray[i].w = rnd(0.1f, 0.25f);
  }

  glBindBuffer(GL_ARRAY_BUFFER, m_posAndSizeBuffers[0]);                                                   
  glBufferData(GL_ARRAY_BUFFER, m_particlesNum*4*sizeof(GLfloat), (GLfloat*)&tempArray[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_posAndSizeBuffers[1]);                                                  
  glBufferData(GL_ARRAY_BUFFER, m_particlesNum*4*sizeof(GLfloat), (GLfloat*)&tempArray[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;



  // init velocity buffers
  //
  float velScale = 0.1f;
  for(int i=0;i<m_particlesNum;i++)
  {
    tempArray[i].x = rnd(-velScale,velScale);
    tempArray[i].y = rnd(-velScale,velScale);
    tempArray[i].z = rnd(-velScale,velScale);
    tempArray[i].w = 0.0f;
  }

  glBindBuffer(GL_ARRAY_BUFFER, m_velAndHPBuffers[0]);                                                   
  glBufferData(GL_ARRAY_BUFFER, m_particlesNum*4*sizeof(GLfloat), (GLfloat*)&tempArray[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_velAndHPBuffers[1]);                                                  
  glBufferData(GL_ARRAY_BUFFER, m_particlesNum*4*sizeof(GLfloat), (GLfloat*)&tempArray[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;

  tempArray = std::vector<float4>(); // free memory
  

  std::vector<float> randSeeds(m_particlesNum);
  for(int i=0;i<m_particlesNum;i++)
    randSeeds[i] = rnd(0.0f, 1.0f);

  glBindBuffer(GL_ARRAY_BUFFER, m_randBuffers[0]);                                                   
  glBufferData(GL_ARRAY_BUFFER, m_particlesNum*sizeof(float), (GLfloat*)&randSeeds[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_randBuffers[1]);                                                  
  glBufferData(GL_ARRAY_BUFFER, m_particlesNum*sizeof(float), (GLfloat*)&randSeeds[0], GL_STATIC_DRAW); CHECK_GL_ERRORS;


  glBindBuffer(GL_ARRAY_BUFFER, 0);  
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
int GPUparticleSystem::AddEmitter(const float a_pos[3], const float a_norm[3], float a_maxAngleCos /* = 0.5 */)
{
  m_emittersPos.push_back(float3(a_pos));
  m_emittersNorm.push_back(float3(a_norm));
  m_emittersCos.push_back(a_maxAngleCos);

  return int(m_emittersPos.size());
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::SetViewAndProjectionMatrices(float a_matrixView[16], float a_matrixProj[16])
{
  memcpy(m_modelViewMatrix,  a_matrixView, 16*sizeof(float));
  memcpy(m_projectionMatrix, a_matrixProj, 16*sizeof(float));
}


///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::Draw()
{
  GLint oldBlendMode,oldDepthMode;
  glGetIntegerv(GL_BLEND, &oldBlendMode);
  glGetIntegerv(GL_DEPTH_TEST, &oldDepthMode);

  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_COLOR, GL_ONE);

  glUseProgram(m_renderProgram.program); CHECK_GL_ERRORS;

  setUniform(m_renderProgram.program, "modelViewMatrix",  float4x4(m_modelViewMatrix));
  setUniform(m_renderProgram.program, "projectionMatrix", float4x4(m_projectionMatrix));  
  setUniform(m_renderProgram.program ,"particleColor", float3(1.0f, 0.75f, 0.0f));

  bindTexture(m_renderProgram.program, 1, "colorTex", m_pFogTexture->GetColorTexId());

  glBindVertexArray(m_drawVAOs[1-m_currPinPongId]); CHECK_GL_ERRORS;
  glDrawArrays(GL_POINTS, 0, m_particlesNum);       CHECK_GL_ERRORS;  

  glDepthMask(GL_TRUE);

  if(!oldBlendMode)
    glDisable(GL_BLEND);

  if(!oldDepthMode)
    glDisable(GL_DEPTH_TEST);
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::DebugDraw()
{

}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void GPUparticleSystem::Process(float delta_t)
{
  UpdateParameters(delta_t);

  glUseProgram(m_animateProgram.program); CHECK_GL_ERRORS;
  setUniform(m_animateProgram.program, "delta_t", delta_t);
  setUniform(m_animateProgram.program, "wind", m_wind);
  //setUniform(m_animateProgram.program, "epicenter", float3(0,1,0));

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

  m_currPinPongId = 1 - m_currPinPongId;
  m_lastDeltaTime = delta_t;
}

