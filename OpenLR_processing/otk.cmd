@ECHO OFF
REM This script enables you to run the OTK without having to specify the 
REM complex call to the jar file which would be:
REM java -jar otk-1.4.2-with-dependencies.jar

SET OTK_JAR=otk-1.4.2-with-dependencies.jar

REM Try to detect a java installation by reading its usage output

SET JAVAFOUND=0
FOR /F %%R IN ('java ^| FIND  "Usage" ') DO (
  SET JAVAFOUND=1
)

if %JAVAFOUND% == 0 (
    echo "Error: java command not found in PATH, OTK requires a Java installation!"
    exit /B 1
)

SET OTK_PATH="%~dp0\%OTK_JAR%"

IF NOT EXIST %OTK_PATH% (

  echo Error: Could not find the otk binary file %OTK_JAR%, assumed here: %OTK_PATH%
  exit /B 1
)


java -jar %OTK_PATH% %*

