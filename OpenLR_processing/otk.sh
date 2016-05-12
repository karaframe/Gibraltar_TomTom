#!/bin/sh

# This script enables you to run the OTK without having to specify the 
# complex call to the jar file which would be:
# java -jar otk-1.4.2-with-dependencies.jar

OTK_JAR=otk-1.4.2-with-dependencies.jar

# Check for java available

if ! which java >/dev/null 2>&1 ; then
  echo "Error: java command not found in PATH, OTK requires a Java installation!"
  exit 1
fi

MY_PATH="`dirname \"$0\"`"

OTK_PATH=$MY_PATH/$OTK_JAR

if [ ! -f $OTK_PATH ];
then
  echo "Error: Could not find the otk binary file $OTK_JAR, assumed here: $OTK_PATH"
  exit 1
fi

java -jar $OTK_PATH $@
