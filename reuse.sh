#!/bin/sh

# SPDX-FileCopyrightText: 2023 "Everybody"
#
# SPDX-License-Identifier: MIT

LIKE_C="$(find -name "*.scala") $(find -name "*.sbt") project/build.properties $(find -name "*.h")  $(find -name "*.c")  $(find -name "*.cpp")  $(find -name "*.v")"
LIKE_PYTHON="$(find -name "*.tcl") $(find -name "*.py") $(find -name "*.sh") $(find -name "makefile")" 
LIKE_RAW="$(find -name "*.license")  $(find -name "*.md") .gitmodules .github/workflows/scala.yml"
ARGS='addheader --copyright-style spdx --license MIT --copyright "Everybody"'

echo $LIKE_C
reuse $ARGS --style c $LIKE_C
reuse $ARGS --style python $LIKE_PYTHON
reuse addheader --license CC0-1.0 --copyright "Everybody"  $LIKE_RAW
reuse addheader --license CC0-1.0 --copyright "Everybody"  $(find -name ".gitignore")

reuse lint
