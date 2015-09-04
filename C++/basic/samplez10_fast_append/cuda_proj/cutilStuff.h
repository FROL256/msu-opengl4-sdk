#pragma once

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

// includes, kernels
#include <cuda_runtime.h>




static inline void __checkCudaErrors( cudaError err, const char *file, const int line )
{
  if( cudaSuccess != err) 
  {
    fprintf(stderr, "%s(%i) : CUDA Runtime API error %d: %s.\n", file, line, (int)err, cudaGetErrorString( err ) );
    exit(-1);
  }
}

static inline void __getLastCudaError( const char *errorMessage, const char *file, const int line )
{
  cudaError_t err = cudaGetLastError();
  if( cudaSuccess != err) 
  {
    fprintf(stderr, "%s(%i) : getLastCudaError() CUDA error : %s : (%d) %s.\n", file, line, errorMessage, (int)err, cudaGetErrorString( err ) );
    exit(-1);
  }
}


// This will output the proper CUDA error strings in the event that a CUDA host call returns an error
#define checkCudaErrors(err) __checkCudaErrors(err, __FILE__, __LINE__)
#define cudaSafeCall(err) __checkCudaErrors(err, __FILE__, __LINE__)
#define cutilSafeCall(err) __checkCudaErrors(err, __FILE__, __LINE__)


#define CUDA_SAFE_CALL(call) {                                    \
  cudaError err = call;                                           \
  if( cudaSuccess != err) {                                       \
  fprintf(stderr, "Cuda error in file '%s' in line %i : %s.\n", __FILE__, __LINE__, cudaGetErrorString( err) ); \
  exit(EXIT_FAILURE);                                           \
  } }


//#ifdef _DEBUG
//#  define CUT_CHECK_ERROR(errorMessage) {                                    \
//  cudaError_t err = cudaGetLastError();                                    \
//  if( cudaSuccess != err) {                                                \
//  fprintf(stderr, "Cuda error: %s in file '%s' in line %i : %s.\n",    \
//  errorMessage, __FILE__, __LINE__, cudaGetErrorString( err) );\
//  exit(EXIT_FAILURE);                                                  \
//  }                                                                        \
//  err = CUT_DEVICE_SYNCHRONIZE();                                           \
//  if( cudaSuccess != err) {                                                \
//  fprintf(stderr, "Cuda error: %s in file '%s' in line %i : %s.\n",    \
//  errorMessage, __FILE__, __LINE__, cudaGetErrorString( err) );\
//  exit(EXIT_FAILURE);                                                  \
//  }                                                                        \
//}
//#else
#define CUT_CHECK_ERROR(errorMessage) {                                    \
  cudaError_t err = cudaGetLastError();                                    \
  if( cudaSuccess != err) {                                                \
  fprintf(stderr, "Cuda error: %s in file '%s' in line %i : %s.\n",    \
  errorMessage, __FILE__, __LINE__, cudaGetErrorString( err) );\
  exit(EXIT_FAILURE);                                                  \
  }                                                                        \
}
//#endif



// This will output the proper error string when calling cudaGetLastError
#define getLastCudaError(msg)      __getLastCudaError (msg, __FILE__, __LINE__)
//#define CUT_CHECK_ERROR(errorMessage) _getLastCudaError (msg, __FILE__, __LINE__)


#ifndef cutil_MIN
#define cutil_MIN(a,b) ((a < b) ? a : b)
#endif
#ifndef cutil_MAX
#define cutil_MAX(a,b) ((a > b) ? a : b)
#endif


// Beginning of GPU Architecture definitions
static int _ConvertSMVer2Cores(int major, int minor)
{
  // Defines for GPU Architecture types (using the SM version to determine the # of cores per SM
  typedef struct {
    int SM; // 0xMm (hexidecimal notation), M = SM Major version, and m = SM minor version
    int Cores;
  } sSMtoCores;

  sSMtoCores nGpuArchCoresPerSM[] = 
  { { 0x10,  8 }, // Tesla Generation (SM 1.0) G80 class
  { 0x11,  8 }, // Tesla Generation (SM 1.1) G8x class
  { 0x12,  8 }, // Tesla Generation (SM 1.2) G9x class
  { 0x13,  8 }, // Tesla Generation (SM 1.3) GT200 class
  { 0x20, 32 }, // Fermi Generation (SM 2.0) GF100 class
  { 0x21, 48 }, // Fermi Generation (SM 2.1) GF10x class
  { 0x30, 192}, // Fermi Generation (SM 3.0) GK10x class
  {   -1, -1 }
  };

  int index = 0;
  while (nGpuArchCoresPerSM[index].SM != -1) 
  {
    if (nGpuArchCoresPerSM[index].SM == ((major << 4) + minor) ) 
    {
      return nGpuArchCoresPerSM[index].Cores;
    }	
    index++;
  }

  printf("MapSMtoCores undefined SM %d.%d is undefined (please update to the latest SDK)!\n", major, minor);
  return -1;
}


// This function returns the best GPU (with maximum GFLOPS)
static int gpuGetMaxGflopsDeviceId()
{
  int current_device   = 0, sm_per_multiproc = 0;
  int max_compute_perf = 0, max_perf_device  = 0;
  int device_count     = 0, best_SM_arch     = 0;
  cudaDeviceProp deviceProp;

  cudaGetDeviceCount( &device_count );

  // Find the best major SM Architecture GPU device
  while ( current_device < device_count ) 
  {
    cudaGetDeviceProperties( &deviceProp, current_device );
    if (deviceProp.major > 0 && deviceProp.major < 9999) 
    {
      best_SM_arch = cutil_MAX(best_SM_arch, deviceProp.major);
    }
    current_device++;
  }

  // Find the best CUDA capable GPU device
  current_device = 0;
  while( current_device < device_count ) 
  {
    cudaGetDeviceProperties( &deviceProp, current_device );
    if (deviceProp.major == 9999 && deviceProp.minor == 9999) 
    {
      sm_per_multiproc = 1;
    } 
    else 
    {
      sm_per_multiproc = _ConvertSMVer2Cores(deviceProp.major, deviceProp.minor);
    }

    int compute_perf  = deviceProp.multiProcessorCount * sm_per_multiproc * deviceProp.clockRate;
    if( compute_perf  > max_compute_perf ) 
    {
      // If we find GPU with SM major > 2, search only these
      if ( best_SM_arch > 2 ) 
      {
        // If our device==dest_SM_arch, choose this, or else pass
        if (deviceProp.major == best_SM_arch) 
        {	
          max_compute_perf  = compute_perf;
          max_perf_device   = current_device;
        }
      } 
      else 
      {
        max_compute_perf  = compute_perf;
        max_perf_device   = current_device;
      }
    }
    ++current_device;
  }

  return max_perf_device;
}

// Initialization code to find the best CUDA Device
static int initCudaDevice()
{
  cudaDeviceProp deviceProp;
  int devID = 0;

  // Otherwise pick the device with highest Gflops/s
  devID = gpuGetMaxGflopsDeviceId();
  checkCudaErrors( cudaSetDevice( devID ) );
  checkCudaErrors( cudaGetDeviceProperties(&deviceProp, devID) );
  printf("> Using CUDA device [%d]: %s\n", devID, deviceProp.name);

  return devID;
}


#define CUDA_CHECK_ERROR(err) if(err != cudaSuccess) {fprintf(stderr, "Failed at line %d (error code %s)!\n", __LINE__, cudaGetErrorString(err)); getchar(); return false; }


