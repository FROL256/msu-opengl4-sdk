//////////////////////////////////////////////////////////////////
// glHelper.h Author: Vladimir Frolov, 2011, Graphics & Media Lab.
//////////////////////////////////////////////////////////////////

#include "glHelper.h"
#include <sstream>

FullScreenQuad::FullScreenQuad()
{
  float quadPos[] =
  {
    -1.0f,  1.0f,	// v0 - top left corner
    -1.0f, -1.0f,	// v1 - bottom left corner
    1.0f,  1.0f,	// v2 - top right corner
    1.0f, -1.0f	  // v3 - bottom right corner
  };

  m_vertexPosBufferObject = 0;
  m_vertexPosLocation = 0; // simple layout, assume have only positions at location = 0

  glGenBuffers(1, &m_vertexPosBufferObject);                                                   OGL_CHECK_FOR_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, m_vertexPosBufferObject);                                      OGL_CHECK_FOR_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, 4*2*sizeof(GLfloat), (GLfloat*)quadPos, GL_STATIC_DRAW);    OGL_CHECK_FOR_ERRORS;

  glGenVertexArrays(1, &m_vertexArrayObject);                                               OGL_CHECK_FOR_ERRORS;
  glBindVertexArray(m_vertexArrayObject);                                                   OGL_CHECK_FOR_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_vertexPosBufferObject);                    OGL_CHECK_FOR_ERRORS;                   
  glEnableVertexAttribArray(m_vertexPosLocation);                            OGL_CHECK_FOR_ERRORS;
  glVertexAttribPointer(m_vertexPosLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);   OGL_CHECK_FOR_ERRORS;

  glBindVertexArray(0);
}


FullScreenQuad::~FullScreenQuad()
{
  if(m_vertexPosBufferObject)
  {
    glDeleteBuffers(1, &m_vertexPosBufferObject);
    m_vertexPosBufferObject = 0;
  }

  if(m_vertexArrayObject)
  {
    glDeleteVertexArrays(1, &m_vertexArrayObject);
    m_vertexArrayObject = 0;
  }

}


void FullScreenQuad::Draw()
{
  glBindVertexArray(m_vertexArrayObject); OGL_CHECK_FOR_ERRORS;
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 8);  OGL_CHECK_FOR_ERRORS;  // 4 vertices with 2 floats per vertex = 8 elements total.
}


void TextureSetup(GLuint program, GLint unit, const GLchar *name, GLuint texture)
{
  glActiveTexture(GL_TEXTURE0 + unit);   OGL_CHECK_FOR_ERRORS;
  glBindTexture(GL_TEXTURE_2D, texture);   OGL_CHECK_FOR_ERRORS;

  GLint location = glGetUniformLocation(program, name);   OGL_CHECK_FOR_ERRORS;
  if(location >=0)
    glUniform1i(location, unit);

  OGL_CHECK_FOR_ERRORS;
}


