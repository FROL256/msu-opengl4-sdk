#include "MyOpenCLProgram.h"

#include <iostream>
#include <fstream>
#include <vector>

//#include <GL/glew.h>
#include <windows.h>


void MyAppCL::Init(const char* a_fileName, int selectedDeviceId)
{
  // load opencl.dll from system32
  //
  int initRes = clewInit(L"opencl.dll");
  if (initRes == -1)
  {
    std::cerr << "clew can not load 'opencl.dll' " << std::endl;
    exit(0);
  }

  std::vector<PlatformDevPair> devList = listAllOpenCLDevices();

  char deviceName[1024];

  // list all devices
  //
  for (size_t i = 0; i < devList.size(); i++)
  {
    memset(deviceName, 0, 1024);
    CHECK_CL(clGetDeviceInfo(devList[i].dev, CL_DEVICE_NAME, 1024, deviceName, NULL));
    std::cerr << "[cl_core]: device name = " << deviceName << std::endl;
  }

  if (selectedDeviceId >= devList.size())
  {
    std::cerr << "wrong opencl id = " << selectedDeviceId << std::endl;
    exit(0);
  }
  cl_int ciErr1 = CL_SUCCESS;

  cl_platform_id pla = devList[selectedDeviceId].platform;
  cl_device_id   dev = devList[selectedDeviceId].dev;

  memset(deviceName, 0, 1024);
  CHECK_CL(clGetDeviceInfo(dev, CL_DEVICE_NAME, 1024, deviceName, NULL));
  std::cerr << std::endl;
  std::cerr << "[cl_core]: using device  : " << deviceName << std::endl;

  std::cout << std::endl;
  std::cout << "[cl_core]: using device id : " << selectedDeviceId << std::endl;
  std::cout << "[cl_core]: using device    : " << deviceName << std::endl;

  // init cl contect and command queue
  //
  this->device   = dev;
  this->platform = pla;
  //this->ctx      = clCreateContext(0, 1, &this->device, NULL, NULL, &ciErr1);

  cl_context_properties properties[] = {
      CL_GL_CONTEXT_KHR,   (cl_context_properties)wglGetCurrentContext(), // WGL Context
      CL_WGL_HDC_KHR,      (cl_context_properties)wglGetCurrentDC(),      // WGL HDC
      CL_CONTEXT_PLATFORM, (cl_context_properties)this->platform,         // OpenCL platform
      0
    };

  this->ctx = clCreateContext(properties, 1, &this->device, NULL, NULL, &ciErr1);

  if (ciErr1 != CL_SUCCESS)
  {
    std::cerr << "[cl_core]: clCreateContext status = " << ciErr1 << std::endl;
    exit(0);
  }

  this->cmdQueue = clCreateCommandQueue(this->ctx, this->device, 0, &ciErr1);

  if (ciErr1 != CL_SUCCESS)
  {
    std::cerr << "[cl_core]: clCreateCommandQueue status = " << ciErr1 << std::endl;
    exit(0);
  }

  // compile cl "shaders"
  //
  std::string options = "-cl-mad-enable -cl-no-signed-zeros -cl-single-precision-constant -cl-denorms-are-zero "; // -cl-uniform-work-group-size 

  try
  {
    std::cerr << "[cl_core]: building opencl program  ... " << std::endl;
    this->programs = CLProgram(this->device, this->ctx, a_fileName, options.c_str());
  }
  catch (const std::runtime_error& e)
  {
    std::cerr << e.what() << std::endl;
    exit(0);
  }


  this->testBuff = clCreateBuffer(this->ctx, CL_MEM_READ_WRITE, 512 * 512 * sizeof(float)*4, NULL, &ciErr1);
  if (ciErr1 != CL_SUCCESS)
  {
    std::cerr << "[cl_core]: fail to allocate buffer = " << ciErr1 << std::endl;
    exit(0);
  }

  // m_cl.runKernel_TestCompaction();
}

void MyAppCL::RegisterGLBuffer(unsigned int a_buffId)
{
  cl_int ciErr1 = 0;
  imgBuff = clCreateFromGLBuffer(this->ctx, CL_MEM_READ_WRITE, a_buffId, &ciErr1); // CL_MEM_WRITE_ONLY

  if (ciErr1 != CL_SUCCESS)
  {
    std::cerr << "MyAppCL::RegisterGLBuffer, clCreateFromGLBuffer failed" << std::endl;
    exit(0);
  }

}

void MyAppCL::UnregisterGLBuffer()
{
  clReleaseMemObject(imgBuff); imgBuff = 0;
}

void MyAppCL::Destroy()
{
  clReleaseMemObject(testBuff); testBuff = 0;
  clReleaseMemObject(imgBuff);  imgBuff  = 0;

  // free all buffers and teztures ... 
  programs.~CLProgram();

  if (this->cmdQueue)
  {
    clReleaseCommandQueue(this->cmdQueue);
    this->cmdQueue = 0;
  }

  if (this->ctx)
  {
    clReleaseContext(this->ctx);
    this->ctx = 0;
  }


}

void MyAppCL::waitIfDebug(const char* file, int line) const
{
#ifdef _DEBUG

  cl_int cErr = clFinish(this->cmdQueue);

  if (cErr != CL_SUCCESS)
  {
    const char* err = getOpenCLErrorString(cErr);
    std::cerr << err << ", " << file << ", line " << line << std::endl;
  }

#endif
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

static inline int blocks(int elems, int threadsPerBlock)
{
  if (elems % threadsPerBlock == 0 && elems >= threadsPerBlock)
    return elems / threadsPerBlock;
  else
    return (elems / threadsPerBlock) + 1;
}

void MyAppCL::GenerateImageDataToGLBuffer(int w, int h, int someIndex, float time)
{
  CHECK_CL(clEnqueueAcquireGLObjects(this->cmdQueue, 1, &imgBuff, 0, 0, 0));

  const int w2 = blocks(w, 16) * 16;
  const int h2 = blocks(h, 16) * 16;

  size_t global_item_size[2] = { w2, h2 };
  size_t local_item_size[2]  = { 16, 16 };

  cl_kernel myKernel = programs.kernel("GenerateSomeProceduralImage");

  CHECK_CL(clSetKernelArg(myKernel, 0, sizeof(cl_mem), (void*)&imgBuff));
  CHECK_CL(clSetKernelArg(myKernel, 1, sizeof(cl_int), (void*)&w));
  CHECK_CL(clSetKernelArg(myKernel, 2, sizeof(cl_int), (void*)&h));
  CHECK_CL(clSetKernelArg(myKernel, 3, sizeof(cl_int), (void*)&someIndex));
  CHECK_CL(clSetKernelArg(myKernel, 4, sizeof(float),  (void*)&time));

  CHECK_CL(clEnqueueNDRangeKernel(this->cmdQueue, myKernel, 2, NULL, global_item_size, local_item_size, 0, NULL, NULL));
  waitIfDebug(__FILE__, __LINE__);

  CHECK_CL(clEnqueueReleaseGLObjects(this->cmdQueue, 1, &imgBuff, 0, 0, 0));
}
