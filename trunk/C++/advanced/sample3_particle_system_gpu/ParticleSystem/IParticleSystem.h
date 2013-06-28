#pragma once

#include "../vsgl3/glHelper.h"

class IParticleSystem
{
public:

  IParticleSystem(){}
  virtual ~IParticleSystem(){}

  virtual void Clear() = 0;
  virtual void Init() = 0;

  virtual void Draw() = 0;
  virtual void DebugDraw() = 0;

  virtual void Process(float delta_t) = 0;

  virtual void SetViewAndProjectionMatrices(float a_matrixView[16], float a_matrixProj[16]) = 0;

  virtual void SetParticlesNum(int a_num) = 0;
  virtual void SetParticlesTexture(GLuint a_glTexId) = 0;
  virtual void SetParticlesSize(float a_size) = 0;
  virtual void SetParticleLifeTime(float a_msecs) = 0;
  virtual void SetParticlesColor(const float a_startColor[3], const float a_endColr[3]) = 0;
  virtual void SetCam(const float a_camPos[3], const float a_camLookAt[3]) = 0;

  virtual void SetForceParticlesNum(int a_num) = 0;
  virtual void SetGravitation(const float a_grav[3]) = 0;
  virtual void SetWind(const float a_wind[3]) = 0;

  virtual int AddEmitter(const float a_pos[3], const float a_norm[3], float a_maxAngleCos = 0.5) = 0;

protected:
  IParticleSystem(const IParticleSystem& rhs);
  IParticleSystem& operator=(const IParticleSystem& rhs);

};