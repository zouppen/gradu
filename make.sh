#!/bin/bash -eu
OUTFILE=lehtonen_siljander_pro_gradu
pdflatex rakenne.tex
bibtex rakenne
pdflatex rakenne.tex
pdflatex -synctex=1 rakenne.tex
mv rakenne.pdf "$OUTFILE.pdf"
mv rakenne.synctex.gz "$OUTFILE.synctex.gz"

echo
echo 'Gradu käännetty!'
echo '----------------'
echo
echo "Tiedosto: $OUTFILE"
echo
