#!/usr/bin/env sh

for i in $* ; do
	dot2tex -o $i.tex $i.dot
	xelatex $i.tex
	open $i.pdf
done