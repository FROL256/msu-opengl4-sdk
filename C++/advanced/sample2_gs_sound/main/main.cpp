/////////////////////////////////////////////////////////////////
// main.cpp Author: Vladimir Frolov, 2011, Graphics & Media Lab.
/////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "..//vsgl3/glHelper.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <windows.h>

#include "bass.h"
#pragma comment(lib, "bass.lib") 

ShaderProgram g_program;
ShaderProgram g_programWireFrame;
ShaderProgram g_postProcessProgram;

ShaderProgram g_extractBrightPixelsProgram;
ShaderProgram g_blurProgram;
ShaderProgram g_bloomProgram;


struct MyInput
{
  MyInput() 
  {
    cam_rot[0] = cam_rot[1] = cam_rot[2] = cam_rot[3] = 0.f;
    mx = my = 0;
    rdown = ldown = false;
    posprocessEffectId = 2;
  }

  int mx;
  int my;
  bool rdown;
  bool ldown;
  float cam_rot[4];
  float cam_pos[4];
  int posprocessEffectId;

}input;

FullScreenQuad* g_pFullScreenQuad = NULL;
SimpleMesh* g_simpleMesh = NULL;
RenderableTexture2D* g_pFullScreenTexture = NULL;


SimpleMesh* g_simpleMeshWire = NULL;
RenderableTexture2D* g_pPinPongTex1 = NULL;
RenderableTexture2D* g_pPinPongTex2 = NULL;

int g_width  = 0;
int g_height = 0;
float3 g_camPos(0,0,6);

float4x4 g_projectionMatrix;

float3 g_matAmbientColors[4];
float3 g_matDiffuseColors[4];
float3 g_matSpecularColors[4];

HSTREAM g_sound = 0;
std::string g_fileName;

void RequreExtentions()
{
  CHECK_GL_ERRORS;

  std::cout << "GPU Vendor: " << glGetString(GL_VENDOR) << std::endl;
  std::cout << "GPU Name  : " << glGetString(GL_RENDERER) << std::endl;
  std::cout << "GL_VER    : " << glGetString(GL_VERSION) << std::endl;
  std::cout << "GLSL_VER  : " << glGetString(GL_SHADING_LANGUAGE_VERSION) << std::endl;

  //GlusHelperRequireExt h;
  //h.require("GL_EXT_texture_filter_anisotropic");
}


struct SoundSpeed
{
  enum {FREQ_NUMBER = 8};
  float freq[FREQ_NUMBER];

  SoundSpeed() {for(int i=0;i<FREQ_NUMBER;i++) freq[i] = 0.0f;}
};

SoundSpeed GetSoundSynchr()
{
  if(g_sound == 0) 
    return SoundSpeed();

  static float g_musicDataBuffer[1024]; 
  BASS_ChannelGetData(g_sound, g_musicDataBuffer, BASS_DATA_FFT2048);
  
  int low = 0; 
  int high = 50;

  int Step=1;
  int stepI = high - low;

  SoundSpeed data;

  for(int j=0;j<SoundSpeed::FREQ_NUMBER;j++)
  {
    float res = 0;

    for(int i=low;i<high;i+=Step)
      res += g_musicDataBuffer[i];
    
    res/=(float(high-low)/float(Step));
    low  += stepI;
    high += stepI;
    
    data.freq[j] = 10*res;
  }

  return data;
}


void CheckBassError(int line)
{
  int errCode = BASS_ErrorGetCode();
  if(errCode!=0) 
    std::cout << "bass err code: " << errCode << "\tline: " << line <<std::endl;
}


GLUSboolean init(GLUSvoid)
{
  try 
  {
    RequreExtentions();

    g_program          = ShaderProgram("../main/Vertex.vert", "../main/TriangleStrip.geom", "../main/Fragment.frag");
    g_programWireFrame = ShaderProgram("../main/Vertex.vert", "../main/LineStrip.geom", "../main/Fragment.frag");
   
    g_postProcessProgram         = ShaderProgram("../main/Quad.vert", "../main/Grayscale.frag");
    g_extractBrightPixelsProgram = ShaderProgram("../main/Quad.vert", "../main/ExtractBrightPixels.frag");
    
    g_blurProgram      = ShaderProgram("../main/Quad.vert", "../main/Blur.frag");
    g_bloomProgram     = ShaderProgram("../main/Quad.vert", "../main/FinalBloomPass.frag");
   

    g_pFullScreenQuad = new FullScreenQuad();
    g_simpleMesh      = new SimpleMesh(g_program.program, 1000);
    g_simpleMeshWire  = new SimpleMesh(g_program.program, 80);

    // GL init done. Now init some our internal stuff, related to materials, cameras and other
    //

    g_matAmbientColors[0]   = float3(0.2, 0.2, 0.2);
    g_matDiffuseColors[0]   = float3(0.5, 0.5, 0.5);
    g_matSpecularColors[0]  = float3(0.75, 0.75, 0.75);

    g_matAmbientColors[1]   = float3(0.0, 0.2, 0.0);
    g_matDiffuseColors[1]   = float3(0.0, 0.5, 0.0);
    g_matSpecularColors[1]  = float3(0.5, 0.5, 0.5);

    g_matAmbientColors[2]   = float3(0.25, 0.25, 0.25);
    g_matDiffuseColors[2]   = float3(0.5, 0.5, 0.5);
    g_matSpecularColors[2]  = float3(0.25, 0.25, 0.25);

    g_matAmbientColors[3]   = float3(0.1, 0.1, 0.1);
    g_matDiffuseColors[3]   = float3(0.5, 0.0, 0.5);
    g_matSpecularColors[3]  = float3(0.5, 0.0, 0.5);


    if(!BASS_Init(1, 44100, 0, 0, 0))
      std::cout << "bass init failed" << std::endl;

    CheckBassError(__LINE__);

    BASS_SetVolume(10.0f);
    
    if(g_fileName != "")
      g_sound = BASS_StreamCreateFile(FALSE, g_fileName.c_str(),0,0,0); 
    else
      g_sound = BASS_StreamCreateFile(FALSE, "../data/03 - Vollmond.mp3",0,0,0);
    
    CheckBassError(__LINE__);

    BASS_StreamPlay(g_sound, FALSE, 0); CheckBassError(__LINE__);

    return GLUS_TRUE;
  }
  catch(std::runtime_error e)
  {
    std::cerr << e.what() << std::endl;
    exit(-1);
  }
  catch(...)
  {
    std::cerr << "Unexpected Exception (init)!" << std::endl;
    exit(-1);
  }
}

/**
* Function to clean up things.
*/
GLUSvoid shutdown(GLUSvoid)
{
  BASS_StreamFree(g_sound);
  BASS_Free();

  delete g_pFullScreenQuad; g_pFullScreenQuad = NULL;
  delete g_simpleMesh; g_simpleMesh = NULL;
  delete g_simpleMeshWire; g_simpleMeshWire = NULL;
}


GLUSvoid reshape(GLUSuint width, GLUSuint height)
{
  try 
  {
    glViewport(0, 0, width, height);
    g_width  = width;
    g_height = height;

    glusPerspectivef(g_projectionMatrix.L(), 40.0f, (GLfloat) width / (GLfloat) height, 1.0f, 100.0f);  // Calculate the projection matrix

    delete g_pFullScreenTexture;
    g_pFullScreenTexture = new RenderableTexture2D(GL_RGBA8,width,height);


    delete g_pPinPongTex1;
    g_pPinPongTex1 = new RenderableTexture2D(GL_RGBA16,width,height);

    delete g_pPinPongTex2;
    g_pPinPongTex2 = new RenderableTexture2D(GL_RGBA16,width,height);

  }
  catch(std::runtime_error e)
  {
    std::cerr << e.what() << std::endl;
    exit(-1);
  }
  catch(...)
  {
    std::cerr << "Unexpected Exception (init)!" << std::endl;
    exit(-1);
  }
}

GLUSvoid mouse(GLUSboolean pressed, GLUSuint button, GLUSuint x, GLUSuint y)
{
  if(!pressed)
    return;

  if (button & 1)// left button
  {
    input.ldown=true;		
    input.mx=x;			
    input.my=y;
  }
  
  if (button & 4)	// right button
  {
    input.rdown=true;
    input.mx=x;
    input.my=y;
  }
}

GLUSvoid mouseMove(GLUSuint button, GLUSint x, GLUSint y)
{
  if(button & 1)		// left button
  {
    int x1 = x;
    int y1 = y;

    input.cam_rot[0] += 0.25f*(y1-input.my);	// change rotation
    input.cam_rot[1] += 0.25f*(x1-input.mx);
    
    input.mx=x;
    input.my=y;
  }
}

GLUSvoid keyboard(GLUSboolean pressed, GLUSuint key)
{
  switch(key)
  {
  case 'w':
  case 'W':
    
    break;

  case 's':
  case 'S':
    
    break;

  case 'a':
  case 'A':
   
    break;

  case 'd':
  case 'D':

    break;

  case '0':
  case ')':
    input.posprocessEffectId = 0;
    break;

  case '1':
  case '!':
    input.posprocessEffectId = 1;
    break;

  case '2':
  case '@':
    input.posprocessEffectId = 2;
    break;

  case '3':
  case '#':
    input.posprocessEffectId = 3;
    break;

  case '4':
  case '$':
    input.posprocessEffectId = 4;
    break;

  }

}

GLUSboolean update(GLUSfloat a_deltaTime)
{
  try 
  {
    static float elaspedTimeFromStart = 0.0f;
    elaspedTimeFromStart += 10*a_deltaTime;
    if(elaspedTimeFromStart > 1e5f) elaspedTimeFromStart = 0.0f;
    
    static float time1 = 0.0f;
    static float time2 = 0.0f;
    static float time3 = 0.0f;

    float vParam1 = 0.01f;
    float vParam2 = 0.01f;
    float vParam3 = 0.01f;

    SoundSpeed soundData = GetSoundSynchr();

    float data3 = 0.5f*(soundData.freq[2] + soundData.freq[3]);
    vParam1 = 5*soundData.freq[0];
    vParam2 = 5*soundData.freq[1];
    vParam3 = 2.5*data3;

    time1 += vParam1*a_deltaTime*200;
    time2 += vParam2*a_deltaTime*200;
    time3 += vParam3*a_deltaTime*200;

    float3 lightPos[2];

    lightPos[0].x = 10*sin(0.15f*elaspedTimeFromStart);
    lightPos[0].y = 10*cos(0.15f*elaspedTimeFromStart);
    lightPos[0].z = 10*sin(0.15f*elaspedTimeFromStart);

    lightPos[1].x = 10*cos(0.15f*elaspedTimeFromStart);
    lightPos[1].y = 10;
    lightPos[1].z = 10*cos(0.15f*elaspedTimeFromStart);

    float4x4 model;
    float4x4 modelView;
    glusLoadIdentityf(model.L()); 
    glusRotateRzRyRxf(model.L(), input.cam_rot[0], input.cam_rot[1], 0.0f);
    glusLookAtf(modelView.L(), g_camPos.x, g_camPos.y, g_camPos.z, 
                               0.0f, 0.0f, 0.0f, 
                               0.0f, 1.0f, 0.0f);                           // ... and the view matrix ...

    glusMultMatrixf(modelView.L(), modelView.L(), model.L()); 	            // ... to get the final model view matrix

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClearDepth(1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    if(input.posprocessEffectId != 0)
      g_pFullScreenTexture->BeginRenderingToThisTexture();

    // make our program current
    //
    glViewport(0, 0, g_width, g_height);
    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    for(int i=0;i<2;i++)
    {
      if(i==0)
        {glUseProgram(g_program.program);  CHECK_GL_ERRORS;  }
      else
        {glUseProgram(g_programWireFrame.program);  CHECK_GL_ERRORS;  }

      setUniform(g_program.program, "modelViewMatrix", modelView);
      setUniform(g_program.program, "projectionMatrix", g_projectionMatrix);  // set matrix we have calculated in "reshape" funtion

      setUniformArray(g_program.program, "g_matAmbientColors", g_matAmbientColors, 4);
      setUniformArray(g_program.program, "g_matDiffuseColors", g_matDiffuseColors, 4);
      setUniformArray(g_program.program, "g_matSpecularColors", g_matSpecularColors, 4);

      setUniformArray(g_program.program, "g_lightPos", lightPos, 2);
      setUniform(g_program.program, "g_camPos", g_camPos);

      setUniform(g_program.program, "timeParameter", 0.00075f*time2);
      setUniform(g_program.program, "timeParameter2", 0.0005f*time1);
     

      float4x4 rotationMatrix;
      glusRotateRzRyRxf(rotationMatrix.L(), 180, 0.0f, 0.0f);
      setUniform(g_program.program, "g_rotorMatrix", rotationMatrix);

      // Now finally draw something
      glEnable(GL_DEPTH_TEST);

      if(i==0)
        g_simpleMesh->Draw();
      else
        g_simpleMeshWire->Draw();
    }

    if(input.posprocessEffectId != 0)
      g_pFullScreenTexture->EndRenderingToThisTexture();

    glDisable(GL_DEPTH_TEST);

    if(input.posprocessEffectId == 1)
    {
      glUseProgram(g_postProcessProgram.program);       
      bindTexture(g_postProcessProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());

      g_pFullScreenQuad->Draw();
    }
    else if(input.posprocessEffectId == 2)
    {
      g_pFullScreenTexture->BuildMipMapChain();

      // extract bright pixels
      //
      g_pPinPongTex1->BeginRenderingToThisTexture();

      glUseProgram(g_extractBrightPixelsProgram.program); 
      bindTexture(g_extractBrightPixelsProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());
    
      g_pFullScreenQuad->Draw();

      g_pPinPongTex1->EndRenderingToThisTexture();
      g_pPinPongTex1->BuildMipMapChain();

      // blur X
      //
      g_pPinPongTex2->BeginRenderingToThisTexture();

      glUseProgram(g_blurProgram.program); 
      bindTexture(g_blurProgram.program, 1, "colorTexture", g_pPinPongTex1->GetColorTexId());
      setUniform  (g_blurProgram.program, "blurDirection", 0);
      setUniform  (g_blurProgram.program, "width", g_width);
      setUniform  (g_blurProgram.program, "height", g_height);
      g_pFullScreenQuad->Draw();

      g_pPinPongTex2->EndRenderingToThisTexture();
      g_pPinPongTex2->BuildMipMapChain();
      
      // blur Y
      //
      g_pPinPongTex1->BeginRenderingToThisTexture();

      bindTexture(g_blurProgram.program, 1, "colorTexture", g_pPinPongTex2->GetColorTexId());
      setUniform  (g_blurProgram.program, "blurDirection", 1);
      setUniform  (g_blurProgram.program, "width", g_width);
      setUniform  (g_blurProgram.program, "height", g_height);

      g_pFullScreenQuad->Draw();

      g_pPinPongTex1->EndRenderingToThisTexture();
      g_pPinPongTex1->BuildMipMapChain();

      // blend final images, make bloom
      //
      glUseProgram(g_bloomProgram.program); 
      bindTexture(g_bloomProgram.program, 1, "colorTexture", g_pFullScreenTexture->GetColorTexId());
      bindTexture(g_bloomProgram.program, 2, "bloomTexture", g_pPinPongTex1->GetColorTexId());

      g_pFullScreenQuad->Draw();
    }

    return GLUS_TRUE;
  }
  catch(std::runtime_error e)
  {
    std::cerr << e.what() << std::endl;
    exit(-1);
  }
  catch(...)
  {
    std::cerr << "Unexpected Exception(render)!" << std::endl;
    exit(-1);
  }
}

/**
 * Main entry point.
 */
int main(int argc, char* argv[])
{
	glusInitFunc(init);
	glusReshapeFunc(reshape);
	glusUpdateFunc(update);
	glusTerminateFunc(shutdown);
  glusMouseFunc(mouse);
  glusMouseMoveFunc(mouseMove);
  glusKeyFunc(keyboard);

	glusPrepareContext(3, 0, GLUS_FORWARD_COMPATIBLE_BIT);

	if (!glusCreateWindow("Frolux Media Player", 640, 480, GLUS_FALSE))
	{
		printf("Could not create window!");
		return -1;
	}

	// Init GLEW
	glewExperimental = GL_TRUE;
  GLenum err=glewInit();
  if(err!=GLEW_OK)
  {
    sprintf("glewInitError", "Error: %s\n", glewGetErrorString(err));
    return -1;
  }
  glGetError(); // flush error state variable, caused by glew errors
  

	// Only continue, if OpenGL 3.3 is supported.
	if (!glewIsSupported("GL_VERSION_3_0"))
	{
		printf("OpenGL 3.0 not supported.");

		glusDestroyWindow();
		return -1;
	}

  if(argc > 1)
  {
    g_fileName = std::string(argv[1]);
    std::cout << "argv[1] = " << g_fileName.c_str() << std::endl;
  }

	glusRun();

	return 0;
}

