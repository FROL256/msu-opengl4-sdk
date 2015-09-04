#ifndef SGARED_BUFFER_GUARDIAN
#define SHARED_BUFFER_GUARDIAN


#include <GL/glew.h>

struct cudaGraphicsResource;

class SharedBuffer
{

public:

  SharedBuffer();
  SharedBuffer(size_t a_byteSize);
  SharedBuffer(void* a_data, size_t a_byteSize);

  virtual ~SharedBuffer();

  inline size_t size() const { return m_byteSize; }
  void resize(size_t a_byteSize);

  void*       mapToCUDA();
  const void* mapToCUDAForRead();
  void*       mapToCUDAForWrite();

  void*       mapToCPU();
  const void* mapToCPUForRead();
  void*       mapToCPUForWrite();

  GLuint      mapToGL();              
  inline void unmapBackToGL() { mapToGL(); } 

  void        update(void* a_data, size_t a_byteSize, size_t offset) {} // TODO: implement this via glBufferSubData

protected:

  SharedBuffer(const SharedBuffer& rhs) {}                              // NOTE: We probably can try to implement reference counter and you only have to copy/share m_glBuff in this case, but ...
  SharedBuffer& operator=(const SharedBuffer& rhs) { return *this; }    //       you probably have to think what happen if you map one copy of the SharedBuffer instance when 
                                                                        //       others are not mapped and they (others) used by the OpenGL => you'll get OpenGL error
                                                                        // 

  void mapTo(int a_state);
  void changeRWFlags(int a_flags, int a_nextState);

  enum MAP_STATES{MAPPED_TO_GL   = 0, 
                  MAPPED_TO_CUDA = 1, 
                  MAPPED_TO_CPU  = 2};

  int    m_currState;
  GLuint m_currRWFlags; // GL_MAP_READ_BIT | GL_MAP_WRITE_BIT

  GLuint m_glBuff;
  int m_bufferAppendFormat;

  void*  m_cudaDevicePointer;
  struct cudaGraphicsResource* m_pCudaResourceStruct;
  void*  m_cpuData;

  size_t m_byteSize;
  
};

void AppendDataWithProgramFromVAO(GLuint a_vao, GLuint resBuffId, int a_elements, GLuint a_program);








#endif


