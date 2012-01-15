@echo off
java -cp "%~dp0\scala-library.jar;%~dp0\scala-compiler.jar;%~dp0\jline.jar" -Dscala.usejavacp=true scala.tools.nsc.Main %*