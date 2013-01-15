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
	rols.tex\
	*tikzDictionary

all:
	make vignettes
	make clean

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif

vignettes: 
	cp rols.Rnw.knitr rols.Rnw
	"$(R_HOME)/bin/R" --vanilla -e "library(knitr); knit2pdf('rols.Rnw');"
	##bibtex rols
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex

clean:
	rm -f $(LATEXFILES)
	rm -f *~
	cp rols.Rnw.stub rols.Rnw
