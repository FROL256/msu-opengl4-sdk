////////////////////////////////////////////////////////////////////////////
//
// Copyright 1993-2013 NVIDIA Corporation.  All rights reserved.
//
// Please refer to the NVIDIA end user license agreement (EULA) associated
// with this source code for terms and conditions that govern your use of
// this software. Any use, reproduction, disclosure, or distribution of
// this software and related documentation outside the terms of the EULA
// is strictly prohibited.
//
////////////////////////////////////////////////////////////////////////////

/* Example of integrating CUDA functions into an existing
 * application / framework.
 * Host part of the device code.
 * Compiled with Cuda compiler.
 */

#include <GL/glew.h>

// System includes
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <iostream>

// CUDA runtime
//
#include <cuda.h>
#include <cuda_runtime_api.h>
#include <cuda_gl_interop.h> 

#include "cutilStuff.h"

#include "SharedBuffer.h"

extern "C" struct cudaGraphicsResource* fromCudaRegisterBufferForRead(GLuint a_buff)         
{ 
  struct cudaGraphicsResource* resource = NULL;
  if( cudaGraphicsGLRegisterBuffer(&resource, a_buff, cudaGraphicsMapFlagsReadOnly) != cudaSuccess)
    return NULL;  
} 

extern "C" struct cudaGraphicsResource* fromCudaRegisterBufferForWrite(GLuint a_buff)        
{ 
  struct cudaGraphicsResource* resource = NULL;
  if( cudaGraphicsGLRegisterBuffer(&resource, a_buff, cudaGraphicsMapFlagsWriteDiscard) != cudaSuccess)
    return NULL; 
  return resource; 
} 

extern "C" struct cudaGraphicsResource* fromCudaRegisterBufferForReadAndWrite(GLuint a_buff) 
{ 
  struct cudaGraphicsResource* resource = NULL;
  if( cudaGraphicsGLRegisterBuffer(&resource, a_buff, cudaGraphicsMapFlagsNone) != cudaSuccess)
    return NULL; 
  return resource; 
} 

extern "C" void  fromCudaUnregisterBuffer(struct cudaGraphicsResource* a_pResource) 
{
  if(a_pResource == NULL)
    return;

  CUDA_SAFE_CALL(cudaGraphicsUnregisterResource(a_pResource));
}

extern "C" void* fromCudaMapRegisteredBuffer(struct cudaGraphicsResource*& a_pGraphRes) 
{ 
  void* buffer = NULL;
  size_t numBytes = 0; 
  
  CUDA_SAFE_CALL(cudaGraphicsMapResources(1, &a_pGraphRes, 0));
  CUDA_SAFE_CALL(cudaGraphicsResourceGetMappedPointer((void**)&buffer, &numBytes, a_pGraphRes));

  return buffer; 
}

extern "C" void fromCudaUnmapRegisteredBuffer(struct cudaGraphicsResource*& a_pGraphRes) 
{
  CUDA_SAFE_CALL(cudaGraphicsUnmapResources(1, &a_pGraphRes, 0));
}



#define BLOCK_SIZE 512


__global__ void append(float* data, int* mask, int* append_index, float* res, int a_size)
{
	const int i(blockIdx.x * BLOCK_SIZE + threadIdx.x);
	if(i>=a_size)
    return;
  
  float val = data[i];
	if(mask[i])
		res[atomicAdd(append_index, 1)] = val;
}

#define NUM_OF_TESTS 10

void make_tests(int ARRAY_SIZE)
{
	srand(unsigned int(__TIME__));
	FILE* fd = fopen("Input.txt", "w");
	for(int j = 0; j < NUM_OF_TESTS; j++)
		for(int i = 0; i < ARRAY_SIZE; i++)
		{
			fprintf(fd, "%d ", rand() > RAND_MAX / 2 ? 1 : 0);
		}
	fclose(fd);
}

extern "C" int makeAppendWithCUDAKernelTest(int ARRAY_SIZE, bool createTests)
{
  float* host_array = new float[ARRAY_SIZE];
	int*   host_mask  = new   int[ARRAY_SIZE];
	

  if(createTests)
  {
    make_tests(ARRAY_SIZE);
    printf("Tests are ready\n");
	}


	FILE* fd = fopen("Input.txt", "r");
	if(fd == NULL)
	{
		printf("Couldn't open the input file\n");
		getchar();
		return 1;
	}

	for(int i = 0; i < ARRAY_SIZE; i++)
	{
		host_array[i] = float(i + 1);
	}

	cudaEvent_t start, stop;
	cudaEventCreate(&start);
	cudaEventCreate(&stop);

  //
  //
  SharedBuffer dataArray(host_array, ARRAY_SIZE * sizeof(float));
  SharedBuffer dataMask(host_mask, ARRAY_SIZE * sizeof(int));
  SharedBuffer dataResult(host_array, ARRAY_SIZE * sizeof(float));

  float* device_array  = (float*)dataArray.mapToCUDA();          // CUDA_CHECK_ERROR(cudaMalloc((void**)&device_array,   ARRAY_SIZE * sizeof(float)));
  int*   device_mask   = (int*)dataMask.mapToCUDA();             // CUDA_CHECK_ERROR(cudaMalloc((void**)&device_mask,    ARRAY_SIZE * sizeof(float)));
  float* device_result = (float*)dataResult.mapToCUDAForWrite(); // CUDA_CHECK_ERROR(cudaMalloc((void**)&device_result,  ARRAY_SIZE * sizeof(float)));
  
  int* device_append_index;
	CUDA_CHECK_ERROR(cudaMalloc((void**)&device_append_index, sizeof(int)));

	float times[NUM_OF_TESTS] = {0.0f};
	float average_time = 0.0f;

	printf("Array size: %d floats \nBlock size: %d threads\nMaking %d tests \n", ARRAY_SIZE, BLOCK_SIZE, NUM_OF_TESTS);

	for(int k = 0; k < NUM_OF_TESTS; k++)
	{
		for(int i = 0; i < ARRAY_SIZE; i++)
			fscanf(fd, "%d", &(host_mask[i]));

		CUDA_CHECK_ERROR(cudaMemcpy(device_array, host_array, ARRAY_SIZE * sizeof(float), cudaMemcpyHostToDevice));
		CUDA_CHECK_ERROR(cudaMemcpy(device_mask,  host_mask,  ARRAY_SIZE * sizeof(int),   cudaMemcpyHostToDevice));

		CUDA_CHECK_ERROR(cudaMemset(device_result,  0, ARRAY_SIZE * sizeof(float)));
		CUDA_CHECK_ERROR(cudaMemset(device_append_index, 0, sizeof(int)));
	
		CUDA_CHECK_ERROR(cudaEventRecord(start, 0));

		append<<<(ARRAY_SIZE / BLOCK_SIZE + 1), BLOCK_SIZE>>>(device_array, device_mask, device_append_index, device_result, ARRAY_SIZE);
		CUDA_CHECK_ERROR(cudaGetLastError());

		CUDA_CHECK_ERROR(cudaEventRecord(stop, 0));
		CUDA_CHECK_ERROR(cudaEventSynchronize(stop));

		CUDA_CHECK_ERROR(cudaEventElapsedTime(&(times[k]), start, stop));

		printf("Time spent executing the test %d by the GPU: %.8f milliseconds\n", k, times[k]);

		average_time += times[k];
	}
	average_time /= float(NUM_OF_TESTS);
	
	printf("Average time: %.8f milliseconds\n", average_time);
	getchar();

	fclose(fd);

  CUDA_CHECK_ERROR(cudaFree(device_append_index));

	CUDA_CHECK_ERROR(cudaEventDestroy(start));
	CUDA_CHECK_ERROR(cudaEventDestroy(stop));

	delete[] host_array;
	delete[] host_mask;

  return 0;
}



