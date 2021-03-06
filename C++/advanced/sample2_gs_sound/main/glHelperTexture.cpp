#include "glHelper.h"
#include <sstream>


RenderableTexture2D::RenderableTexture2D(GLenum format, GLsizei width, GLsizei height)
{
  m_colorTexture = TextureCreateEmpty(format, GL_RGBA, width, height);
  m_depthTexture = TextureCreateEmpty(GL_DEPTH_COMPONENT24, GL_DEPTH_COMPONENT, width, height);

  GLenum fboStatus;                           // ���������� ��� �������� ��������� FBO
  glGenFramebuffers(1, &m_fbo);               OGL_CHECK_FOR_ERRORS;
  glBindFramebuffer(GL_FRAMEBUFFER, m_fbo);   OGL_CHECK_FOR_ERRORS;

  // ������������ ��������� �������� � �������� FBO
  glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, m_colorTexture, 0); OGL_CHECK_FOR_ERRORS;
  glFramebufferTexture(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT,  m_depthTexture, 0); OGL_CHECK_FOR_ERRORS;

  // �������� ������� FBO �� ������������
  if ((fboStatus = glCheckFramebufferStatus(GL_FRAMEBUFFER)) != GL_FRAMEBUFFER_COMPLETE)
  {
    std::stringstream errMsg;
    errMsg << "glCheckFramebufferStatus error = " << fboStatus << "line " << __LINE__ << ", file " << __FILE__ << std::endl;
    throw std::runtime_error(errMsg.str().c_str());
  }

  // ������ �������� ��������� FBO
  glBindFramebuffer(GL_FRAMEBUFFER, 0); OGL_CHECK_FOR_ERRORS;
}

RenderableTexture2D::~RenderableTexture2D()
{
  glDeleteTextures(1, &m_colorTexture); m_colorTexture = -1;
  glDeleteTextures(1, &m_depthTexture); m_depthTexture = -1;

  glDeleteFramebuffers(1, &m_fbo);
}

// �������� "������" �������� � ������� �����������
GLuint RenderableTexture2D::TextureCreateEmpty(GLint internalFormat, GLenum format, GLsizei width, GLsizei height)
{
  GLuint texture;

  // �������� � OpenGL ��������� ������ ��������
  glGenTextures(1, &texture);

  // ������� �������� ��������
  glBindTexture(GL_TEXTURE_2D, texture);

  // ��������� ��������� ���������� �������� - �������� ���������� 
  // GL_LINEAR_MIPMAP_LINEAR - ���� ����� � �������� ���� ���-��� ������
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);  OGL_CHECK_FOR_ERRORS;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); OGL_CHECK_FOR_ERRORS;

  // ��������� ��������� "������������" �������� - ���������� ������������
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);  OGL_CHECK_FOR_ERRORS;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);  OGL_CHECK_FOR_ERRORS;

  // ������� "������" ��������
  glTexImage2D(GL_TEXTURE_2D, 0, internalFormat, width, height, 0, format, GL_UNSIGNED_BYTE, NULL); OGL_CHECK_FOR_ERRORS;

  // �������� �� ������� ������
  OGL_CHECK_FOR_ERRORS;

  return texture;
}



void RenderableTexture2D::BeginRenderingToThisTexture()
{
  glBindFramebuffer(GL_FRAMEBUFFER, m_fbo); OGL_CHECK_FOR_ERRORS;              // ������������� �������� FBO
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);  // ���������� ������� ������� �����, ������� � ���������
}


void RenderableTexture2D::EndRenderingToThisTexture()
{
  glBindFramebuffer(GL_FRAMEBUFFER, 0); OGL_CHECK_FOR_ERRORS;
}

void RenderableTexture2D::BuildMipMapChain()
{
  glBindTexture(GL_TEXTURE_2D, m_colorTexture);
  glGenerateMipmap(GL_TEXTURE_2D);
}


