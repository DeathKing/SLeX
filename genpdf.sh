#!/usr/bin/env sh

dot2tex -o $1.tex $1.dot
xelatex $1.tex
open $1.pdf
