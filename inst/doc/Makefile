whoami=$(shell whoami)

all: clean RamiGO

RamiGO:
	cp -f vignette-src/RamiGO.Rnw .
	"${R_HOME}/bin/R" --vanilla -e "library(pgfSweave); pgfSweave('RamiGO.Rnw');"
	pdflatex RamiGO
	pdflatex RamiGO
	pdflatex RamiGO
	if test -d cache; then rm -fr cache; fi
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.tikz *.toc *.map *.sh *.dvi
	rm -f RamiGO-*.pdf example.*
	cp -f vignette-src/RamiGO-fake.Rnw RamiGO.Rnw

clean:
	if test -d cache; then rm -fr cache; fi
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.tikz *.toc *.map *.sh *.dvi
	rm -f RamiGO-*.pdf example.*
	cp -f vignette-src/RamiGO-fake.Rnw RamiGO.Rnw

pdfclean:
	rm -f RamiGO.pdf