Some of the functions in rols are documented with the [roxygen2](https://github.com/klutometis/roxygen
([CRAN package](http://cran.r-project.org/web/packages/roxygen2/index.html). 
The respective Rd pages have been created with
```
R --vanilla -e "library(roxygen2); roxygenize('"$(PKG)"', roclets=\"rd\")";
```

Note that only the `rd` roclet has been used, i.e. only `roxygen`'s Rd-generation 
functionality. `roxygen` can also generate the NAMSPACE (roclet `namespace`) and 
parts if the DESCRIPTION (previous and roclet `collate`) files, which is not used here. 
If all roclets are used, some native data will be overwritten.
