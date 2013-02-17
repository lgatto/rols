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
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex	
	bibtex rols
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex	
	"$(R_HOME)/bin/R" CMD pdflatex rols.tex	

setvars:
ifeq (${R_HOME},)
R_HOME=	$(shell R RHOME)
endif


clean:
	rm -f $(LATEXFILES)
	rm -f *~
