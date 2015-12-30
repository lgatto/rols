
> The onology lookup service, the backend behind rols has been
> [updated](http://www.ebi.ac.uk/ols/beta/roadmap.html). As a result,
> the rols package is being re-implemented and will undergo changes in
> its user interface. The current interface will be maintained for
> Bioconductor release 3.2 and will be discontinued afterwards. Please
> see the
> [`rols` version 2.0 branch](https://github.com/lgatto/rols/tree/v2.0)
> for the new release.

### The Ontology Lookup Service 

The [Ontology Lookup Service](http://www.ebi.ac.uk/ontology-lookup/) (OLS), developed at the [European Bioinformatics Institute](http://www.ebi.ac.uk/) (EBI), is a unified web interface to query multiple ontologies from a single location. It also provides Simple Object Access Protocol (SOAP) webservice for programmatic access.

### The `rols` R package

The `rols` package uses functionality from the [SSOAP](http://www.omegahat.org/SSOAP/) and [XMLSchema](http://www.omegahat.org/XMLSchema/) packages to query the OLS directly from R. It also uses some functionality from the [Biobase](http://bioconductor.org/packages/release/bioc/html/Biobase.html) package.

### The `CVParam` class

The `CVParam` S4 class allows to generate controlled vocabulary parameters for all 88 ontologies through the OLS, as used in the frame of the [Proteomics Standards Initiative](http://www.psidev.info/) (PSI) effort. 

### Installation 
`rols` is available from the [Bioconductor](http://www.bioconductor.org) repository. The package and its dependencies can be installed 
```
source("http://www.bioconductor.org/biocLite.R")
biocLite("rols")
```
See also the `rols` [Bioconductor page](http://bioconductor.org/packages/2.11/bioc/html/rols.html) for on-line access to the vignette and the reference manual.

The code on [github](https://github.com/lgatto/rols) is for sharing, testing, issue tracking and forking/pulling purposes. The **official source code** is available from the [Bioconductor svn server](https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/). Get is with 
```
svn co https://hedgehog.fhcrc.org/bioconductor/trunk/madman/Rpacks/rols
```
(user name: `readonly`, password: `readonly`)

 
### Help
`rols` comes with plenty of documentation. Have a start with the vignette ``vignette("rols", package="rols")``. Do not hesitate to contact me for questions/comments/suggestions.


### Building from source

Some of the functions in rols are documented with the [roxygen2](https://github.com/klutometis/roxygen)
([CRAN package](http://cran.r-project.org/web/packages/roxygen2/index.html)). 
The respective Rd pages have been created with
```
R --vanilla -e "library(roxygen2); roxygenize('"$(PKG)"', roclets=\"rd\")";
```

Note that only the `rd` roclet has been used, i.e. only `roxygen`'s Rd-generation 
functionality. `roxygen` can also generate the NAMESPACE (`namespace` roclet) and 
parts of (?) the DESCRIPTION (previous and `collate` roclet) files, which is not used here. 
If all roclets are used, some original data will be overwritten.
