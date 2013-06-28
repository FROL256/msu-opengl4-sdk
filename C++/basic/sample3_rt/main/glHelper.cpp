//////////////////////////////////////////////////////////////////
// glHelper.h Author: Vladimir Frolov, 2011, Graphics & Media Lab.
//////////////////////////////////////////////////////////////////

#include "glHelper.h"

FullScreenQuad::FullScreenQuad()
{
  float quadPos[] =
  {
    -1.0f,  1.0f,	// v0 - top left corner
    -1.0f, -1.0f,	// v1 - bottom left corner
    1.0f,  1.0f,	// v2 - top right corner
    1.0f, -1.0f	  // v3 - bottom right corner
  };

  m_vertexBufferObject = 0;
  m_vertexLocation = 0; // simple layout, assume have only positions at location = 0

  glGenBuffers(1, &m_vertexBufferObject);                                                   CHECK_GL_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, m_vertexBufferObject);                                      CHECK_GL_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, 4*2*sizeof(GLfloat), (GLfloat*)quadPos, GL_STATIC_DRAW);    CHECK_GL_ERRORS;

  glGenVertexArrays(1, &m_vertexArrayObject);                                               CHECK_GL_ERRORS;
  glBindVertexArray(m_vertexArrayObject);                                                   CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_vertexBufferObject);                    CHECK_GL_ERRORS;                   
  glEnableVertexAttribArray(m_vertexLocation);                            CHECK_GL_ERRORS;
  glVertexAttribPointer(m_vertexLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);   CHECK_GL_ERRORS;

  glBindVertexArray(0);
}


FullScreenQuad::~FullScreenQuad()
{
  if(m_vertexBufferObject)
  {
    glDeleteBuffers(1, &m_vertexBufferObject);
    m_vertexBufferObject = -1;
  }

  if(m_vertexArrayObject)
  {
    glDeleteVertexArrays(1, &m_vertexArrayObject);
    m_vertexArrayObject = -1;
  }
}


void FullScreenQuad::Draw()
{
  glBindVertexArray(m_vertexArrayObject); CHECK_GL_ERRORS;
  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);  CHECK_GL_ERRORS; 
}

