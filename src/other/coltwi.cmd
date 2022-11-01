@ECHO off

SetLocal EnableDelayedExpansion

rem   _____      _             _       _   _______       _ _ _       _     _
rem  / ____|    | |           (_)     | | |__   __|     (_) (_)     | |   | |
rem | |     ___ | | ___  _ __  _  __ _| |    | |_      ___| |_  __ _| |__ | |_
rem | |    / _ \| |/ _ \| '_ \| |/ _` | |    | \ \ /\ / / | | |/ _` | '_ \| __|
rem | |___| (_) | | (_) | | | | | (_| | |    | |\ V  V /| | | | (_| | | | | |_
rem  \_____\___/|_|\___/|_| |_|_|\__,_|_|    |_| \_/\_/ |_|_|_|\__, |_| |_|\__|
rem                                                             __/ |
rem                                                            |___/
rem This script is used to run the scala implementation of the
rem solo AI for Colonial Twilight

rem Set the current working directory to the directory where this script is running.
rem This is important so that all of our relative path references work correctly.
pushd %~dp0

set CLASSPATH=lib\coltwi_2.13-2.7.jar;^
lib\scala-library-2.13.10.jar;^
lib\scala-parser-combinators_2.13-2.1.1.jar

java -cp %CLASSPATH%  coltwi.ColonialTwilight %*

popd
EndLocal
