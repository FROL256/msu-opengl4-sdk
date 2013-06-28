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

  glGenBuffers(1, &m_vertexPosBufferObject);                                                   CHECK_GL_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, m_vertexPosBufferObject);                                      CHECK_GL_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, 4*2*sizeof(GLfloat), (GLfloat*)quadPos, GL_STATIC_DRAW);    CHECK_GL_ERRORS;

  glGenVertexArrays(1, &m_vertexArrayObject);                                               CHECK_GL_ERRORS;
  glBindVertexArray(m_vertexArrayObject);                                                   CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_vertexPosBufferObject);                    CHECK_GL_ERRORS;                   
  glEnableVertexAttribArray(m_vertexPosLocation);                            CHECK_GL_ERRORS;
  glVertexAttribPointer(m_vertexPosLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);   CHECK_GL_ERRORS;

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
  glBindVertexArray(m_vertexArrayObject); CHECK_GL_ERRORS;
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 8);  CHECK_GL_ERRORS;  // 4 vertices with 2 floats per vertex = 8 elements total.
}


void bindTexture(GLuint program, GLint unit, const GLchar *name, GLuint texture)
{
  glActiveTexture(GL_TEXTURE0 + unit);
  glBindTexture(GL_TEXTURE_2D, texture);

  GLint location = glGetUniformLocation(program, name);
  if(location >=0)
    glUniform1i(location, unit);

  CHECK_GL_ERRORS;
}


