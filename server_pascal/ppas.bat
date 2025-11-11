@echo off
SET THEFILE=D:\UCT\Cuarto Semestre\Sistemas Inteligentes y Interfaces Graficas\S.I_Proyecto_2\server_pascal\server_main.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections   --subsystem windows --entry=_WinMainCRTStartup    -o "D:\UCT\Cuarto Semestre\Sistemas Inteligentes y Interfaces Graficas\S.I_Proyecto_2\server_pascal\server_main.exe" "D:\UCT\Cuarto Semestre\Sistemas Inteligentes y Interfaces Graficas\S.I_Proyecto_2\server_pascal\link13428.res"
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
