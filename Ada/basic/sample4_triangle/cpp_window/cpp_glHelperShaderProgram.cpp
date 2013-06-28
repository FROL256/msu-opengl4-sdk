//////////////////////////////////////////////////////////////////
// glHelper.h Author: Vladimir Frolov, 2012, Graphics & Media Lab.
//////////////////////////////////////////////////////////////////

#include <GL/glew.h>

#include "GL/glus.h"
#include "cpp_window.h"
#include "cpp_glHelper.h"

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

#include <sstream>
#include <fstream>
#include <string>
#include <vector>
#include <stdexcept>

void LoadTextFromFile(const std::string& a_fileName, std::string& a_shaderSource);

ShaderProgram::ShaderProgram()
{
  program = 0;
  vertex  = 0;
  control = 0;
  evaluation = 0;
  geometry   = 0;
  fragment   = 0;
}

std::string CutPathToFolder(const std::string& a_pathToFile)
{
  size_t posOfLastSlash = a_pathToFile.find_last_of('\\');

  if(posOfLastSlash == std::string::npos)
    posOfLastSlash = a_pathToFile.find_last_of('/');

  if(posOfLastSlash == std::string::npos)
    return "";

  return a_pathToFile.substr(0, posOfLastSlash);
}

std::string GetFileNameFromIncludeExpr(const std::string& a_str, size_t a_pos)
{
   size_t first  = a_str.find_first_of('"', a_pos);
   size_t second = a_str.find_first_of('"', first+1);

   if(first == std::string::npos || second == std::string::npos)
   {
     first  = a_str.find_first_of('<', a_pos);
     second = a_str.find_first_of('>', first+1);
   }

   if(first == std::string::npos || second == std::string::npos)
     return "";

   return a_str.substr(first+1, second-first-1);
}

void ReplaceIncludeWithFile(std::string& a_str, size_t a_pos, const std::string& a_fileName)
{
  std::ifstream sourceFile(a_fileName.c_str());

  std::string source;

  sourceFile.seekg(0, std::ios::end);
  source.reserve(sourceFile.tellg());
  sourceFile.seekg(0, std::ios::beg);
  source.assign((std::istreambuf_iterator<char>(sourceFile)), std::istreambuf_iterator<char>());

  size_t endOfLine = a_str.find_first_of('\n', a_pos);

  a_str = a_str.substr(0,a_pos) + source + a_str.substr(endOfLine, a_str.size());
}


void IncludeFiles(std::string& a_shader, const std::string& a_pathToFile)
{
  size_t found = 0;

  while(true)
  {
    found = a_shader.find("#include", found);
    bool commented = false;

    // look back and see whether it was commented or nor
    //
    if(found != std::string::npos)
    {
      for(int i=1;i<10;i++)
      {
        if(a_shader[found-i] == '\n')
          break;

        if(a_shader[found-i]=='/' && a_shader[found-i-1]=='/')
        {
          commented = true;
          break;
        }
      }
    }

    if(found != std::string::npos && !commented)
    {
      std::string pathToFolder = CutPathToFolder(a_pathToFile);
      std::string fileName = GetFileNameFromIncludeExpr(a_shader, found);

      ReplaceIncludeWithFile(a_shader, found, pathToFolder + "/" + fileName);
      found = 0;
    }
    else if(commented)
    {
      found++;
    }
    else
      break;
  };
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////
void LoadTextFromFile(const std::string& a_fileName, std::string& a_shaderSource)
{
  std::ifstream vertSourceFile(a_fileName.c_str());

  if(!vertSourceFile.is_open())
    throw std::runtime_error(std::string("cant open file: ") + a_fileName);

  vertSourceFile.seekg(0, std::ios::end);
  a_shaderSource.reserve(vertSourceFile.tellg());

  vertSourceFile.seekg(0, std::ios::beg);
  a_shaderSource.assign((std::istreambuf_iterator<char>(vertSourceFile)), std::istreambuf_iterator<char>());

  IncludeFiles(a_shaderSource, a_fileName);
}

////////////////////////////////////////////////////////////////////////////////////
////
bool ShaderProgram::Link()
{
  GLint linked;

  glLinkProgram(program);
  glGetProgramiv(program, GL_LINK_STATUS, &linked);

  if (!linked)
  {
    GLint logLength, charsWritten;
    glGetProgramiv(this->program, GL_INFO_LOG_LENGTH, &logLength);

    char* log = new char [logLength];
    glGetProgramInfoLog(this->program, logLength, &charsWritten, log);

    fprintf(stderr, "Shader program link error:\n");
    fprintf(stderr, "%s\n", log);

    delete [] log;
    program = -1;
    return false;
  }

  return true;
}


////////////////////////////////////////////////////////////////////////////////////
////
bool ShaderProgram::Compile(const char** vertexSource, const char** controlSource, const char** evaluationSource, const char** geometrySource, const char** fragmentSource)
{
  GLint compiled;
  GLint logLength, charsWritten;
  char* log;

  if(!vertexSource)
    return false;

  program = 0;
  vertex  = 0;
  control = 0;
  evaluation = 0;
  geometry   = 0;
  fragment   = 0;

  vertex = glCreateShader(GL_VERTEX_SHADER);

  glShaderSource(vertex, 1, (const char**)vertexSource, 0);
  glCompileShader(vertex);
  glGetShaderiv(vertex, GL_COMPILE_STATUS, &compiled);

  if (!compiled)
  {
    glGetShaderiv(vertex, GL_INFO_LOG_LENGTH, &logLength);
    log = (char*)malloc(logLength);
    glGetShaderInfoLog(vertex, logLength, &charsWritten, log);

    printf("Vertex shader compile error:\n");
    printf("%s\n", log);

    free(log);

    vertex = 0;

    return false;
  }

  if (controlSource)
  {
    control = glCreateShader(GL_TESS_CONTROL_SHADER);
    glShaderSource(control, 1, (const char**)controlSource, 0);
    glCompileShader(control);
    glGetShaderiv(control, GL_COMPILE_STATUS, &compiled);

    if (!compiled)
    {
      glGetShaderiv(control, GL_INFO_LOG_LENGTH, &logLength);
      log = (char*)malloc(logLength);
      glGetShaderInfoLog(control, logLength, &charsWritten, log);

      printf("Control shader compile error:\n");
      printf("%s\n", log);

      free(log);

      control = 0;

      return false;
    }
  }

  if (evaluationSource)
  {
    evaluation = glCreateShader(GL_TESS_EVALUATION_SHADER);
    glShaderSource(evaluation, 1, (const char**)evaluationSource, 0);
    glCompileShader(evaluation);
    glGetShaderiv(evaluation, GL_COMPILE_STATUS, &compiled);

    if (!compiled)
    {
      glGetShaderiv(evaluation, GL_INFO_LOG_LENGTH, &logLength);
      log = (char*)malloc(logLength);
      glGetShaderInfoLog(evaluation, logLength, &charsWritten, log);

      printf("Evaluation shader compile error:\n");
      printf("%s\n", log);

      free(log);

      evaluation = 0;

      return false;
    }
  }


  if (geometrySource)
  {
    geometry = glCreateShader(GL_GEOMETRY_SHADER);
    glShaderSource(geometry, 1, (const char**)geometrySource, 0);
    glCompileShader(geometry);
    glGetShaderiv(geometry, GL_COMPILE_STATUS, &compiled);

    if (!compiled)
    {
      glGetShaderiv(geometry, GL_INFO_LOG_LENGTH, &logLength);

      log = (char*)malloc(logLength);
      glGetShaderInfoLog(geometry, logLength, &charsWritten, log);

      printf("Geometry shader compile error:\n");
      printf("%s\n", log);

      free(log);

      geometry = 0;

      return false;
    }
  }


  if(fragmentSource)
  {
    fragment = glCreateShader(GL_FRAGMENT_SHADER);

    glShaderSource(fragment, 1, (const char**)fragmentSource, 0);
    glCompileShader(fragment);
    glGetShaderiv(fragment, GL_COMPILE_STATUS, &compiled);

    if (!compiled)
    {
      glGetShaderiv(fragment, GL_INFO_LOG_LENGTH, &logLength);

      log = (char*)malloc(logLength);

      glGetShaderInfoLog(fragment, logLength, &charsWritten, log);

      printf("Fragment shader compile error:\n");
      printf("%s\n", log);

      free(log);

      fragment = 0;

      return false;
    }
  }


  program = glCreateProgram();

  if(vertex)      glAttachShader(program, vertex);
  if(control)     glAttachShader(program, control);
  if(evaluation)  glAttachShader(program, evaluation);
  if(geometry)    glAttachShader(program, geometry);
  if(fragment)    glAttachShader(program, fragment);

  return true;
}


extern "C" ShaderProgram cpp_create_shader_program(const char* vs, const char* ts, const char* es, const char* gs, const char* ps)
{
  ShaderProgram prog;

  try
  {

  std::string vertSource;
  std::string tessSource;
  std::string evalSource;
  std::string geomSource;
  std::string fragSource;

  if(vs!=NULL && strlen(vs) > 0) LoadTextFromFile(vs, vertSource);
  if(ts!=NULL && strlen(ts) > 0) LoadTextFromFile(ts, tessSource);
  if(es!=NULL && strlen(es) > 0) LoadTextFromFile(es, evalSource);
  if(gs!=NULL && strlen(gs) > 0) LoadTextFromFile(gs, geomSource);
  if(ps!=NULL && strlen(ps) > 0) LoadTextFromFile(ps, fragSource);

  const char* tmpVertSource = vertSource.c_str();
  const char* tmpTessSource = tessSource.c_str();
  const char* tmpEvalSource = evalSource.c_str();
  const char* tmpGeomSource = geomSource.c_str();
  const char* tmpFragSource = fragSource.c_str();

  bool compiled = false;

  if(strlen(vs) > 0 && strlen(ts) > 0 && strlen(es) > 0 && strlen(gs) > 0 && strlen(ps) > 0)
  {
    compiled = prog.Compile(&tmpVertSource, &tmpTessSource, &tmpEvalSource, &tmpGeomSource, &tmpFragSource);
  }
  else if(strlen(vs) > 0 && strlen(gs) > 0 && strlen(ps) > 0)
  {
    compiled = prog.Compile(&tmpVertSource, NULL, NULL, &tmpGeomSource, &tmpFragSource);
  }
  else if(strlen(vs) > 0 && strlen(ps) > 0)
  {
    compiled = prog.Compile(&tmpVertSource, NULL, NULL, NULL, &tmpFragSource);
  }
  else if(strlen(vs) > 0)
  {
    compiled = prog.Compile(&tmpVertSource, NULL, NULL, NULL, NULL);
  }
  else
    throw std::runtime_error("all shader sources == NULL");

  if(!compiled)
    throw std::runtime_error("shader compilation failed");

  if(!prog.Link())
      throw std::runtime_error("shader linkage failed");

  }
  catch(std::runtime_error e)
  {
    std::cout << e.what() << std::endl;
    memset(&prog, sizeof(ShaderProgram), 0);
  }
  catch(...)
  {
    std::cout << "cpp_create_shader_program, unknown C++ exception occured!" << std::endl;
    memset(&prog, sizeof(ShaderProgram), 0);
  }

  return prog;
}




