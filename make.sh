#!/bin/bash -e

pdflatex rakenne.tex
bibtex rakenne
pdflatex rakenne.tex
pdflatex rakenne.tex

echo
echo '----------------'
echo 'Gradu käännetty!'
echo '----------------'
