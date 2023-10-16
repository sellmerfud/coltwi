#! /usr/bin/env bash

# This script is sourced by the package.sh script
# We use this to set two shell variables that are used throughout
# package.sh.  This allows the package.sh script to be generic and
# shared among several projects.

export program_name=coltwi
export main_class=src/main/scala/coltwi/ColonialTwilight.scala
export jarfile_prefix=coltwi