TIMEOUT /T 1
set /p Input=Plak de URL:
 
"C:\Program Files\R\R-3.5.1\bin\i386\Rscript.exe" "C:\Users\pieter_verschelde\Desktop\R_Archief_Labels.R" %Input%
TIMEOUT /T 10