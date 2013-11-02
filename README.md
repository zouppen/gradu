# Gradu

Kirjoittaneet: Joel Lehtonen ja Kristian Siljander.

Valmistui 18.10.2010. Teoksen tunniste on URN:NBN:fi:jyu-201011023052 ja
PDF-muotoinen julkaisu on saatavissa osoitteesta
http://urn.fi/URN:NBN:fi:jyu-201011023052.

## Gradun kääntäminen

    pdflatex rakenne.tex
    bibtex rakenne
    pdflatex rakenne.tex
    pdflatex rakenne.tex

Tai käytä `make.sh`-skriptiä. Kahta ensimmäistä komentoa ei tarvitsisi
ajaa kuin silloin, kun lähdeluettelo muuttuu, mutta niiden ajamisesta
ei haittaakaan ole.

## Joelin graduseminaari

Graduseminaari on pidetty 25.2.2010 ja siihen on lohkaistu luku
dynaamisista WWW-teknologioista (web20.tex). Tiedostoon on siksi
upotettu vähän ehdollisen kääntämisen sääntöjä, jotta samaa lukua voi
käyttää seminaarissa ja varsinaisen raportin osana. Kuitenkin
myöhemmin sisältö varmastikin muuttuu, joten voi olla tarvetta nähdä
seminaarin aikainen versio. Seminaarissa esitelty versio on
719cbb86af6712692de76fd0686753f8e45a5e4c ja seminaariraportin korjattu
versio on 760a3d8fa774e5aa08702111ffcb610978d4d70b.

Seminaariraportti käännetään symbolisen linkin kautta, eli ajamalla
graduhakemiston juuressa:

    pdflatex seminaari_joel.tex. 

Windowsissa tämä voi olla vaikeampaa, pitää ehkä kopoida tiedosto
alihakemistosta käsin.
