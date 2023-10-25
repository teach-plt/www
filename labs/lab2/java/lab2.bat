REM A simple batch program to run PLT lab2 on Windows systems.

REM Adds the current dir to the class path so that lab2.class
REM is found even when called from another directory.

@echo off
set dir=%~dp0
java -cp "%dir%;%CLASSPATH%" lab2 %*
