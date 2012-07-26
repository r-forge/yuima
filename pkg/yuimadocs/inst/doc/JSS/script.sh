#!/bin/bash
date
before="$(date +%s)"
echo 'SWEAVE | PDFLATEX | BIBTEX | PDFLATEX | PDFLATEX. Custom engine--Akasurak-16Nov2009'
#Updated 20Jul2010 for auto including Sweave.sty


export PATH=$PATH:/usr/texbin:/usr/local/bin

R CMD Sweave "$1"
rm *.out
R CMD pdflatex "${1%.*}"
bibtex  "${1%.*}.aux"
rm *.out
R CMD pdflatex "${1%.*}"
rm *.out
R CMD pdflatex "${1%.*}"

open "$1.pdf"

after="$(date +%s)"
elapsed_seconds="$(expr $after - $before)"
date
echo Elapsed time '(m:s)': $(date -r $elapsed_seconds +%M:%S)
