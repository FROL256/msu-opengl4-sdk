//////////////////////////////////////////////////////////////////
// glHelper.h Author: Vladimir Frolov, 2011, Graphics & Media Lab.
//////////////////////////////////////////////////////////////////

#include "glHelper.h"
#include <sstream>

SimpleMesh::SimpleMesh(GLuint a_programId)
{
  glusCreateSpheref(&m_glusShape, 1.0f, 256);

  m_vertexPosBufferObject = 0;

  m_vertexPosLocation       = glGetAttribLocation(a_programId, "vertex");
  m_vertexNormLocation      = glGetAttribLocation(a_programId, "normal");
  m_vertexTexCoordsLocation = glGetAttribLocation(a_programId, "texCoord");

  int maxVertexAttributes = 0;
  glGetIntegerv(GL_MAX_VERTEX_ATTRIBS, &maxVertexAttributes);

  // create buffers a,d fill them with data

  // vertex positions
  //
  glGenBuffers(1, &m_vertexPosBufferObject);                                                   CHECK_GL_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, m_vertexPosBufferObject);                                      CHECK_GL_ERRORS;    
  glBufferData(GL_ARRAY_BUFFER, m_glusShape.numberVertices * 4 * sizeof(GLfloat), (GLfloat*) m_glusShape.vertices, GL_STATIC_DRAW);  CHECK_GL_ERRORS;

  // vertex normals
  //
  glGenBuffers(1, &m_vertexNormBufferObject);                                                   CHECK_GL_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, m_vertexNormBufferObject);                                      CHECK_GL_ERRORS;    
  glBufferData(GL_ARRAY_BUFFER, m_glusShape.numberVertices * 3 * sizeof(GLfloat), (GLfloat*) m_glusShape.normals, GL_STATIC_DRAW);  CHECK_GL_ERRORS;

  // vertex texture coordinates
  //
  glGenBuffers(1, &m_vertexTexCoordsBufferObject);                                                   CHECK_GL_ERRORS;
  glBindBuffer(GL_ARRAY_BUFFER, m_vertexTexCoordsBufferObject);                                      CHECK_GL_ERRORS;    
  glBufferData(GL_ARRAY_BUFFER, m_glusShape.numberVertices * 2 * sizeof(GLfloat), (GLfloat*) m_glusShape.texCoords, GL_STATIC_DRAW);  CHECK_GL_ERRORS;

  // index buffer
  //
  glGenBuffers(1, &m_indexBufferObject);   
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_indexBufferObject);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, m_glusShape.numberIndices * sizeof(GLuint), (GLuint*)m_glusShape.indices, GL_STATIC_DRAW);


  // create VAO and bind each buffer to appropriate "pointer", called vertex array attribute
  //
  glGenVertexArrays(1, &m_vertexArrayObject);                                               CHECK_GL_ERRORS;
  glBindVertexArray(m_vertexArrayObject);                                                   CHECK_GL_ERRORS;

  if(m_vertexPosLocation < maxVertexAttributes)
  {
    glBindBuffer(GL_ARRAY_BUFFER, m_vertexPosBufferObject);                    CHECK_GL_ERRORS;                   
    glEnableVertexAttribArray(m_vertexPosLocation);                            CHECK_GL_ERRORS;
    glVertexAttribPointer(m_vertexPosLocation, 4, GL_FLOAT, GL_FALSE, 0, 0);   CHECK_GL_ERRORS;
  }

  if(m_vertexNormLocation < maxVertexAttributes)
  {
    glBindBuffer(GL_ARRAY_BUFFER, m_vertexNormBufferObject);                    CHECK_GL_ERRORS;                   
    glEnableVertexAttribArray(m_vertexNormLocation);                            CHECK_GL_ERRORS;
    glVertexAttribPointer(m_vertexNormLocation, 3, GL_FLOAT, GL_FALSE, 0, 0);   CHECK_GL_ERRORS;
  }

  if(m_vertexTexCoordsLocation < maxVertexAttributes)
  {
    glBindBuffer(GL_ARRAY_BUFFER, m_vertexTexCoordsBufferObject);                    CHECK_GL_ERRORS;                   
    glEnableVertexAttribArray(m_vertexTexCoordsLocation);                            CHECK_GL_ERRORS;
    glVertexAttribPointer(m_vertexTexCoordsLocation, 2, GL_FLOAT, GL_FALSE, 0, 0);   CHECK_GL_ERRORS;
  }

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, m_indexBufferObject);  CHECK_GL_ERRORS;

  glBindVertexArray(0); // unbind VAO
}


void SimpleMesh::Draw()
{
  glBindVertexArray(m_vertexArrayObject); CHECK_GL_ERRORS;
  glDrawElements(GL_TRIANGLES, m_glusShape.numberIndices, GL_UNSIGNED_INT, 0);  CHECK_GL_ERRORS; 
}


SimpleMesh::~SimpleMesh()
{

  if(m_vertexPosBufferObject)
  {
    glDeleteBuffers(1, &m_vertexPosBufferObject);
    m_vertexPosBufferObject = 0;
  }

  if(m_vertexNormBufferObject)
  {
    glDeleteVertexArrays(1, &m_vertexNormBufferObject);
    m_vertexNormBufferObject = 0;
  }

  if(m_vertexTexCoordsBufferObject)
  {
    glDeleteVertexArrays(1, &m_vertexTexCoordsBufferObject);
    m_vertexTexCoordsBufferObject = 0;
  }

  if(m_indexBufferObject)
  {
    glDeleteVertexArrays(1, &m_indexBufferObject);
    m_indexBufferObject = 0;
  }

  if(m_vertexArrayObject)
  {
    glDeleteVertexArrays(1, &m_vertexArrayObject);
    m_vertexArrayObject = 0;
  }

  glusDestroyShapef(&m_glusShape);

}

