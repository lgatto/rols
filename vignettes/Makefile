B1;2802;0cLATEXFILES = *.aux\
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
	## do not forget to add pgfSweave to Suggests!
	## "$(R_HOME)/bin/R" --vanilla -e "library(pgfSweave); pgfSweave('rols.Rnw');"
	"$(R_HOME)/bin/R" CMD Sweave rols.Rnw 
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex
	bibtex rols
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex
clean:
	rm -f $(LATEXFILES)
	rm -f *~
