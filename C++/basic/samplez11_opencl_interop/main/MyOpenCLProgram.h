#include "../vsgl3/clHelper.h"

void InitOpenCL();

struct MyAppCL
{
  MyAppCL() : ctx(0), cmdQueue(0), platform(0), device(0), width(0), height(0), imgBuff(0) {}

  cl_context       ctx;        // OpenCL context
  cl_command_queue cmdQueue;   // OpenCL command que
  cl_platform_id   platform;   // OpenCL platform
  cl_device_id     device;     // OpenCL device

  CLProgram programs;

  int width;
  int height;
  cl_mem imgBuff;
  cl_mem testBuff;


  void Init(const char* a_fileName, int selectedDeviceId = 0);
  void Destroy();

  void RegisterGLBuffer(unsigned int a_buffId);
  void UnregisterGLBuffer();

  void waitIfDebug(const char* file, int line) const;

  void GenerateImageDataToGLBuffer(int w, int h, int someIndex, float time);

};

