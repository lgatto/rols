
### The Ontology Lookup Service 

The [Ontology Lookup Service](http://www.ebi.ac.uk/ontology-lookup/)
(OLS), developed at the
[European Bioinformatics Institute](http://www.ebi.ac.uk/) (EBI), is a
unified web interface to query multiple ontologies from a single
location. The
[new version](http://www.ebi.ac.uk/ols/beta/roadmap.html) of the OLS
uses a REST API. 

The `rols` package uses functionality from the
[httr](https://cran.r-project.org/web/packages/httr/) package to query
the OLS directly from R.

#### Changes in version 1.99.0

See section *Changes in `rols` 2.0* in the
[vignette](https://github.com/lgatto/rols/blob/master/vignettes/rols.Rmd).

### The `CVParam` class

The `CVParam` S4 class allows to generate controlled vocabulary
parameters for all 88 ontologies through the OLS, as used in the frame
of the [Proteomics Standards Initiative](http://www.psidev.info/)
(PSI) effort.

### Installation

`rols` is available from the
[Bioconductor](http://www.bioconductor.org) repository. The package
and its dependencies can be installed

```
source("http://www.bioconductor.org/biocLite.R")
biocLite("rols")
```

See also the `rols`
[Bioconductor page](http://bioconductor.org/packages/release/bioc/html/rols.html)
for on-line access to the vignette and the reference manual.

### Help

* `rols` comes with plenty of documentation. Have a start with the
  vignette ``vignette("rols", package="rols")``.

* Please direct your questions to the
  [Bioconductor support site](https://support.bioconductor.org/).

* For feature requests and bug reports, use
  [GitHub issues](https://github.com/lgatto/rols/issues)

