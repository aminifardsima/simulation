# Microsoft Developer Studio Project File - Name="00_module var" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=00_module var - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "00_module var.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "00_module var.mak" CFG="00_module var - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "00_module var - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "00_module var - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "00_module var - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "00_module var - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "00_module var - Win32 Release"
# Name "00_module var - Win32 Debug"
# Begin Source File

SOURCE=".\00_module var.f90"
# End Source File
# Begin Source File

SOURCE=.\01_start.f90
DEP_F90_01_ST=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\05_shabakebandi.f90
DEP_F90_05_SH=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\06_TEST.f90
DEP_F90_06_TE=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\07_pre_sub.f90
DEP_F90_07_PR=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\08_dPhincdn_func1.f90
DEP_F90_08_DP=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\08_Phiinc_func1.f90
DEP_F90_08_PH=\
	".\Debug\var.mod"\
	{$(INCLUDE)}"IMSL.mod"\
	
# End Source File
# Begin Source File

SOURCE=".\15 _I_FUNC.f90"
DEP_F90_15__I=\
	".\Debug\var.mod"\
	{$(INCLUDE)}"IMSL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\20_MATRIX.f90
DEP_F90_20_MA=\
	".\Debug\var.mod"\
	{$(INCLUDE)}"IMSL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\22_PHIs.f90
DEP_F90_22_PH=\
	".\Debug\var.mod"\
	{$(INCLUDE)}"IMSL.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\23_PHIi.f90
DEP_F90_23_PH=\
	".\Debug\var.mod"\
	{$(INCLUDE)}"IMSL.mod"\
	
# End Source File
# Begin Source File

SOURCE=".\50_Field OUT.F90"
DEP_F90_50_FI=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\51_parash.f90
DEP_F90_51_PA=\
	".\Debug\var.mod"\
	
# End Source File
# Begin Source File

SOURCE=".\90_main program.f90"
DEP_F90_90_MA=\
	".\Debug\var.mod"\
	
# End Source File
# End Target
# End Project
