#pragma once

#include "IParticleSystem.h"
#include <iostream>
#include <vector>

class GPUparticleSystem : public IParticleSystem
{
public:

  GPUparticleSystem();
  GPUparticleSystem(int a_particlesNum);
  ~GPUparticleSystem();

  void Clear();
  void Init();

  void Draw();
  void DebugDraw();
  void Process(float delta_t);

  void SetViewAndProjectionMatrices(float a_matrixView[16], float a_matrixProj[16]);

  void SetParticlesNum(int a_num) {m_particlesNum = a_num;}
  void SetParticlesTexture(GLuint a_glTexId) {m_particlesTexId = a_glTexId;}
  void SetParticlesSize(float a_size) {m_particlesSize = a_size;}
  void SetParticleLifeTime(float a_msecs) {m_particlesLifeTime = a_msecs;}
  void SetParticlesColor(const float a_startColor[3], const float a_endColor[3]) {m_particlesBeginColor = float3(a_startColor); m_particlesEndColor = float3(a_endColor);}
  void SetCam(const float a_camPos[3], const float a_camLookAt[3]) {m_camPos = float3(a_camPos); m_camLookAt = float3(a_camLookAt); }

  void SetForceParticlesNum(int a_num){}
  void SetGravitation(const float a_grav[3]) {m_grav = float3(a_grav);}
  void SetWind(const float a_wind[3]) {m_wind = float3(a_wind);}

  int AddEmitter(const float a_pos[3], const float a_norm[3], float a_maxAngleCos = 0.5);

protected:

  GPUparticleSystem(const GPUparticleSystem& rhs);
  GPUparticleSystem& operator=(const GPUparticleSystem& rhs);

  virtual void UpdateParameters(float a_delta){}

  void CreateBuffers(GLuint renderProgram, GLuint animateProgram);

  // particles data
  //
  GLuint m_posAndSizeBuffers[2];
  GLuint m_velAndHPBuffers[2];
  GLuint m_randBuffers[2];
  GLuint m_drawVAOs[2];
  GLuint m_animVAOs[2];

  GLuint m_particlesTexId;

  int m_currPinPongId;

  FullScreenQuad* m_pFullScreenQuad;

  Texture2D* m_pFogTexture;

  ShaderProgram m_renderProgram;
  ShaderProgram m_animateProgram;

  float m_modelViewMatrix[16];
  float m_projectionMatrix[16];

  // common particle system parameters
  //
  int m_particlesNum;
  float m_particlesSize;
  float m_particlesLifeTime;
  float3 m_particlesBeginColor;
  float3 m_particlesEndColor;
  float3 m_camPos;
  float3 m_camLookAt;
  float3 m_wind;
  float3 m_grav;

  std::vector<float3> m_emittersPos;
  std::vector<float3> m_emittersNorm;
  std::vector<float>  m_emittersCos;

  float m_lastDeltaTime;
};

