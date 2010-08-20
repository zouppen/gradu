#!/bin/bash -eu
INFILE=tietoturvaluento
OUTFILE=webtietoturva
pdflatex $INFILE.tex
bibtex $INFILE
pdflatex $INFILE.tex
pdflatex -synctex=1 $INFILE.tex
cat $INFILE.pdf >"$OUTFILE.pdf"
mv $INFILE.synctex.gz "$OUTFILE.synctex.gz"

echo
echo 'Artikkeli käännetty!'
echo '--------------------'
echo
echo "Tiedosto: $OUTFILE"
echo
