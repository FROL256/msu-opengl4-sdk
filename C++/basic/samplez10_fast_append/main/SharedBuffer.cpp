#include "SharedBuffer.h"

#include "GL/glus.h"
#include "../vsgl3/glHelper.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <windows.h>

#include <vector>




SharedBuffer::SharedBuffer(size_t a_byteSize) : m_currState  (MAPPED_TO_GL), 
                                                m_cudaDevicePointer(NULL), 
                                                m_pCudaResourceStruct(NULL),
                                                m_cpuData            (NULL), 
                                                m_byteSize(a_byteSize), 
                                                m_currRWFlags(0),
                                                m_bufferAppendFormat(0)
{
  glGenBuffers(1, &m_glBuff);                                       CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_glBuff);                          CHECK_GL_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, a_byteSize, NULL, GL_STATIC_DRAW ); CHECK_GL_ERRORS; // resize buffer
  glBindBuffer(GL_ARRAY_BUFFER, 0);                                 CHECK_GL_ERRORS;
}


SharedBuffer::SharedBuffer() : m_currState(MAPPED_TO_GL), 
                               m_cudaDevicePointer(NULL),
                               m_pCudaResourceStruct(NULL),
                               m_cpuData(NULL), 
                               m_byteSize(0), 
                               m_currRWFlags(0),
                               m_bufferAppendFormat(0)
{
  unsigned int data[1] = {0};

  glGenBuffers(1, &m_glBuff);                              CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_glBuff);                 CHECK_GL_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, 4, data, GL_STATIC_DRAW ); CHECK_GL_ERRORS; // resize buffer
  glBindBuffer(GL_ARRAY_BUFFER, 0);                        CHECK_GL_ERRORS;
}

SharedBuffer::SharedBuffer(void* a_data, size_t a_byteSize) : m_currState(MAPPED_TO_GL), 
                                                              m_cudaDevicePointer(NULL), 
                                                              m_pCudaResourceStruct(NULL),
                                                              m_cpuData(NULL), 
                                                              m_byteSize(a_byteSize), 
                                                              m_currRWFlags(0),
                                                              m_bufferAppendFormat(0)
{
  glGenBuffers(1, &m_glBuff);                                         CHECK_GL_ERRORS;

  glBindBuffer(GL_ARRAY_BUFFER, m_glBuff);                            CHECK_GL_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, a_byteSize, a_data, GL_STATIC_DRAW ); CHECK_GL_ERRORS; // resize buffer
  glBindBuffer(GL_ARRAY_BUFFER, 0);                                   CHECK_GL_ERRORS;
}


SharedBuffer::~SharedBuffer()
{
  unmapBackToGL();
  if(m_glBuff!=0) glDeleteBuffers(1, &m_glBuff); 
}


void SharedBuffer::resize(size_t a_byteSize) 
{
  glBindBuffer(GL_ARRAY_BUFFER, m_glBuff);                          CHECK_GL_ERRORS;
  glBufferData(GL_ARRAY_BUFFER, a_byteSize, NULL, GL_STATIC_DRAW ); CHECK_GL_ERRORS; // resize buffer
  glBindBuffer(GL_ARRAY_BUFFER, 0);                                 CHECK_GL_ERRORS;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


void*  SharedBuffer::mapToCUDA() 
{ 
  changeRWFlags(GL_MAP_READ_BIT | GL_MAP_WRITE_BIT, MAPPED_TO_CUDA);
  this->mapTo(MAPPED_TO_CUDA);   
  return m_cudaDevicePointer; 
}  

void*  SharedBuffer::mapToCPU()  
{ 
  changeRWFlags(GL_MAP_READ_BIT | GL_MAP_WRITE_BIT, MAPPED_TO_CPU);
  this->mapTo(MAPPED_TO_CPU);    
  return m_cpuData;     
}


GLuint SharedBuffer::mapToGL()   
{ 
  this->mapTo(MAPPED_TO_GL);     
  return m_glBuff;      
}

const void* SharedBuffer::mapToCUDAForRead()  
{ 
  changeRWFlags(GL_MAP_READ_BIT, MAPPED_TO_CUDA);
  this->mapTo(MAPPED_TO_CUDA);
  return m_cudaDevicePointer; 
}

void* SharedBuffer::mapToCUDAForWrite() 
{ 
  changeRWFlags(GL_MAP_WRITE_BIT, MAPPED_TO_CUDA);
  this->mapTo(MAPPED_TO_CUDA);
  return m_cudaDevicePointer;
}

const void* SharedBuffer::mapToCPUForRead()  
{ 
  changeRWFlags(GL_MAP_READ_BIT, MAPPED_TO_CPU);
  this->mapTo(MAPPED_TO_CPU); 
  return m_cpuData; 
}


void* SharedBuffer::mapToCPUForWrite() 
{ 
  changeRWFlags(GL_MAP_WRITE_BIT, MAPPED_TO_CPU);
  this->mapTo(MAPPED_TO_CPU); 
  return m_cpuData; 
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

void SharedBuffer::changeRWFlags(int a_flags, int a_nextState)
{
  if(a_flags == m_currRWFlags || a_flags == 0)
    return;

  // was used in past for both read and write. 
  // Don't change RW flags because it can be used for both read and write again in future.
  //
  if(m_currRWFlags & (GL_MAP_READ_BIT | GL_MAP_WRITE_BIT) )
    return;

  if(a_nextState == MAPPED_TO_CPU)
  {
    m_currRWFlags = a_flags;
  }
  else if(a_nextState == MAPPED_TO_CUDA)
  {
	  /*
    fromCudaUnregisterBuffer(m_pCudaResourceStruct);
    
    if(a_flags & (GL_MAP_READ_BIT | GL_MAP_WRITE_BIT))
      m_pCudaResourceStruct = fromCudaRegisterBufferForReadAndWrite(m_glBuff);
    else if(a_flags & GL_MAP_WRITE_BIT)
      m_pCudaResourceStruct = fromCudaRegisterBufferForWrite(m_glBuff);
    else
      m_pCudaResourceStruct = fromCudaRegisterBufferForRead(m_glBuff);
	  */

  }
  else
  {

  }
  

}


void SharedBuffer::mapTo(int a_state)
{
  if(a_state == m_currState)
    return;

  // now 
  //
  switch(a_state)
  {
  case MAPPED_TO_GL:

    if(m_currState == MAPPED_TO_CPU)
    {
      glBindBuffer(GL_ARRAY_BUFFER, m_glBuff); CHECK_GL_ERRORS;
      glUnmapBuffer(GL_ARRAY_BUFFER);          CHECK_GL_ERRORS;
      glBindBuffer(GL_ARRAY_BUFFER, 0);        CHECK_GL_ERRORS;
      m_cpuData = NULL;
    }
    else if(m_currState == MAPPED_TO_CUDA)
    {
      //fromCudaUnmapRegisteredBuffer(m_pCudaResourceStruct);
      m_cudaDevicePointer = NULL;
    }
    break;

  case MAPPED_TO_CPU:
    {
      if(m_currState != MAPPED_TO_GL)
        unmapBackToGL();

      glBindBuffer(GL_ARRAY_BUFFER, m_glBuff);                                     CHECK_GL_ERRORS;
      m_cpuData = glMapBufferRange(GL_ARRAY_BUFFER, 0, m_byteSize, m_currRWFlags); CHECK_GL_ERRORS;
      glBindBuffer(GL_ARRAY_BUFFER, 0);                                            CHECK_GL_ERRORS;
    }
    break;

  case MAPPED_TO_CUDA:
    
    if(m_currState != MAPPED_TO_GL)
      unmapBackToGL();

    //m_cudaDevicePointer = fromCudaMapRegisteredBuffer(m_pCudaResourceStruct);

    break;

  default:
    return;
  }

  m_currState = a_state;
}


void AppendDataWithProgramFromVAO(GLuint a_vao,     // from buffers attached to this vao
                                  GLuint resBuffId, // to this buffer
                                  int a_elements,   // how many elements 
                                  GLuint a_program)
{
  glUseProgram(a_program); CHECK_GL_ERRORS;

  glBindBufferBase(GL_TRANSFORM_FEEDBACK_BUFFER, 0, resBuffId); CHECK_GL_ERRORS;

  glEnable(GL_RASTERIZER_DISCARD);                  CHECK_GL_ERRORS;
  glBeginTransformFeedback(GL_POINTS);              CHECK_GL_ERRORS;

  glBindVertexArray(a_vao);                         CHECK_GL_ERRORS;
  glDrawArrays(GL_POINTS, 0, a_elements);           CHECK_GL_ERRORS;  
  glBindVertexArray(0); 

  glEndTransformFeedback();                         CHECK_GL_ERRORS;
  glDisable(GL_RASTERIZER_DISCARD);                 CHECK_GL_ERRORS;
}




