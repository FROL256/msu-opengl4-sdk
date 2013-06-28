//////////////////////////////////////////////////////////////////
// glHelper.h Author: Vladimir Frolov, 2011, Graphics & Media Lab.
//////////////////////////////////////////////////////////////////

#include "glHelper.h"
#include <sstream>


RenderableTexture2D::RenderableTexture2D(GLenum format, GLsizei width, GLsizei height)
{
  m_colorTexture = TextureCreateEmpty(format, GL_RGBA, width, height);
  m_depthTexture = TextureCreateEmpty(GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT, width, height);

  GLenum fboStatus;                           // переменная для хранения состояния FBO
  glGenFramebuffers(1, &m_fbo);               CHECK_GL_ERRORS;
  glBindFramebuffer(GL_FRAMEBUFFER, m_fbo);   CHECK_GL_ERRORS;

  // присоединяем созданные текстуры к текущему FBO
  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_colorTexture, 0); CHECK_GL_ERRORS;
  glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,  m_depthTexture, 0); CHECK_GL_ERRORS;

  // проверим текущий FBO на корректность
  if ((fboStatus = glCheckFramebufferStatus(GL_FRAMEBUFFER)) != GL_FRAMEBUFFER_COMPLETE)
  {
    std::stringstream errMsg;
    errMsg << "glCheckFramebufferStatus error = " << fboStatus << "line " << __LINE__ << ", file " << __FILE__ << std::endl;
    throw std::runtime_error(errMsg.str().c_str());
  }

  // делаем активным дефолтный FBO
  glBindFramebuffer(GL_FRAMEBUFFER, 0); CHECK_GL_ERRORS;
}

RenderableTexture2D::~RenderableTexture2D()
{
  glDeleteTextures(1, &m_colorTexture); m_colorTexture = -1;
  glDeleteTextures(1, &m_depthTexture); m_depthTexture = -1;

  glDeleteFramebuffers(1, &m_fbo);
}

// создание "пустой" текстуры с нужными параметрами
GLuint RenderableTexture2D::TextureCreateEmpty(GLint internalFormat, GLenum format, GLsizei width, GLsizei height)
{
  GLuint texture;

  // запросим у OpenGL свободный индекс текстуры
  glGenTextures(1, &texture);

  // сделаем текстуру активной
  glBindTexture(GL_TEXTURE_2D, texture);

  // установим параметры фильтрации текстуры - линейная фильтрация
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  // установим параметры "оборачивания" текстуры - отсутствие оборачивания
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  // создаем "пустую" текстуру
  glTexImage2D(GL_TEXTURE_2D, 0, internalFormat, width, height, 0, format, GL_UNSIGNED_BYTE, NULL);

  // проверим на наличие ошибок
  CHECK_GL_ERRORS;

  return texture;
}



void RenderableTexture2D::BeginRenderingToThisTexture()
{
  glBindFramebuffer(GL_FRAMEBUFFER, m_fbo); CHECK_GL_ERRORS;              // устанавливаем активный FBO
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);  // производим очистку буферов цвета, глубины и трафарета
}


void RenderableTexture2D::EndRenderingToThisTexture()
{
  glBindFramebuffer(GL_FRAMEBUFFER, 0); CHECK_GL_ERRORS;
}


