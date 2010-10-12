#!/bin/bash -eu
INFILE=rakenne_kandi
OUTFILE=lehtonen_kandi
pdflatex $INFILE.tex
bibtex $INFILE
pdflatex $INFILE.tex
pdflatex -synctex=1 $INFILE.tex
cat $INFILE.pdf >"$OUTFILE.pdf"
mv $INFILE.synctex.gz "$OUTFILE.synctex.gz"
rm $INFILE.pdf

echo
echo 'Artikkeli käännetty!'
echo '--------------------'
echo
echo "Tiedosto: $OUTFILE"
echo
