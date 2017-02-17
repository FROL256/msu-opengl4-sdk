import re
from zipfile import *
from operator import truth
from os import listdir
from os import path
from os import chdir
from time import *

EXTENSIONS = ["\.h","\.c","\.cpp","\.hcc","\.sln","\.vcproj","\.cu","\.cuh","\.rules","\.py","\.pl","\.rc",
              "\.vert", "\.frag", "\.geom", "\.hlsl", "\.glsl", "\.fx", "\.vs", "\.ps", "\.gs", "\.hs", "\.ds", "\.ts", "\.es", 
              "\.ads", "\.adb", "\.gpr", "\.pas", "\.txt", "\.asm", "\.def"]

EXCEPTION_LIST = ["aux.cu", "aux.cuh", ".svn"]   

PROJECT_NAME = "vsgl3DebugSample"           
curr_path = "."  # this is the path from where program start scan folders            
              
exprStr = ""
for extension in EXTENSIONS:
  exprStr += extension + "|"        
exprStr = exprStr[0:len(exprStr)-1] 
  
filter_src = re.compile(exprStr, re.I) # this variable is the filter for file extensions of files that will be in archive

def arch_recursive(folder, zip):
  files  = listdir(folder)
  for file in files:
    if file in EXCEPTION_LIST:
      continue
    file_name = folder + "\\" + file
    if path.isdir(file_name): 
      #print file_name
      arch_recursive(file_name,zip)
    elif truth(filter_src.search(file)):
      zip.write(file_name)
		
arch_name   = PROJECT_NAME
folder_name = curr_path
arch_name += strftime("_%d_%B_%Y",localtime())		
arch_name += ".zip"
zip = ZipFile(arch_name,'w', ZIP_DEFLATED)
print "Archive name: " + arch_name
arch_recursive(folder_name, zip)

zip.write("External/lib/glew32.lib")
zip.write("Binaries/glew32.dll")

zip.write("Binaries/DevIL.dll")
zip.write("Binaries/ILU.dll")
zip.write("Binaries/ILUT.dll")
