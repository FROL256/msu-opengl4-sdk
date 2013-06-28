#pragma once

#include "GPUParticleSystem.h"
#include <iostream>
#include <vector>


class FireParticleSystem : public GPUparticleSystem
{
public:

  FireParticleSystem(int a_particlesNumber);
  ~FireParticleSystem();

  void Process(float delta_t);

protected:

  void UpdateParameters(float a_delta);
  float m_windChangeTime;

  GLuint m_vertexPosTBO;
};
