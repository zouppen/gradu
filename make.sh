#!/bin/bash -eu
OUTFILE=lehtonen_siljander_pro_gradu.pdf
pdflatex rakenne.tex
bibtex rakenne
pdflatex rakenne.tex
pdflatex rakenne.tex
mv rakenne.pdf "$OUTFILE"

echo
echo 'Gradu käännetty!'
echo '----------------'
echo
echo "Tiedosto: $OUTFILE"
echo
