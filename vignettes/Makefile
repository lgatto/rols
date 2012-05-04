LATEXFILES = *.aux\
        *.bbl\
        *.blg\
        *.ilg\
        *.log\
        *.nlo\
        *.nls\
        *.toc\
        *.aux\
	Rplots.pdf\
	*.dvi\
	*.map\
        *.out\

all:
	make vignettes
	make clean

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif

vignettes: 
	"$(R_HOME)/bin/R" --vanilla -e "library(pgfSweave); pgfSweave('rols.Rnw');"
	bibtex rols
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex
clean:
	rm -f $(LATEXFILES)
	rm -f *~
