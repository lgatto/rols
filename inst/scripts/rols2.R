## Development script for rols version 2.

## http://www.ebi.ac.uk/ols/beta/docs/api
## http://www.ebi.ac.uk/ols/beta/roadmap.html

## "ontologies"

## > ls("package:rols")
##  [1] "allIds"               "as.character.CVParam" "as.character.Map"    
##  [4] "as.character.mapItem" "charIsCVParam"        "childrenRelations"   
##  [7] "CVParam"              "isIdObsolete"         "key"                 
## [10] "map"                  "olsQuery"             "olsVersion"          
## [13] "ontologyLoadDate" "ontologyNames"       
## [16] "parents"              "rootId"               "show"                
## [19] "term"                 "termMetadata"         "termXrefs"           
## [22] "value"               

library("httr")

setGeneric("olsPrefix", function(object) standardGeneric("olsPrefix"))
setGeneric("olsDesc", function(object) standardGeneric("olsDesc"))
setGeneric("olsTitle", function(object) standardGeneric("olsTitle"))

.getOntologies <- function() {
    n <- 150
    x <- GET(paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies?page=0&size=", n))
    warn_for_status(x)
    cx <- content(x)
    if (cx$page$totalElements > n) {
        n <- cx$page$totalElements
        x <- GET(paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies?page=0&size=", n))
        warn_for_status(x)
        cx <- content(x)
    }        
    ans <- lapply(cx[["_embedded"]][[1]], .makeOntology)
    names(ans) <- sapply(ans, olsPrefix)
    ## -- Iterating
    ## .next <- cx[["_links"]][["next"]][[1]]
    ## while (!is.null(.next)) {
    ##     x <- GET(.next)
    ##     warn_for_status(x)
    ##     cx <- content(x)
    ##     ans <- append(ans, lapply(cx[["_embedded"]][[1]], Ontology))
    ##     .next <- cx[["_links"]][["next"]][[1]]
    ## }
    .Ontologies(x = ans)
}

setClassUnion("NullOrChar", c("NULL", "character"))

.Ontology <- setClass("Ontology",
                      slots = c(loaded = "NullOrChar",
                                updated = "NullOrChar",
                                status = "NullOrChar",
                                message = "NullOrChar",
                                version = "NullOrChar",
                                numberOfTerms = "integer",
                                numberOfProperties = "integer",
                                numberOfIndividuals = "integer",
                                config = "list"
                                ))

.Ontologies <- setClass("Ontologies", slots = c(x = "list"))


Ontology <- function(x) {
    url <- paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies/", x)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    .makeOntology(cx)
}

.makeOntology <- function(x)
    .Ontology(loaded = x$loaded,
              updated = x$updated,
              status = x$status,
              message = x$message,
              version = x$version,
              numberOfTerms = x$numberOfTerms,
              numberOfProperties = x$numberOfProperties,
              numberOfIndividuals = x$numberOfIndividuals,
              config = x$config)


setMethod("show", "Ontology",
          function(object) {
              cat("Ontology: ", olsTitle(object), " (", olsPrefix(object) , ")", sep = "")
              cat("   ", strwrap(olsDesc(object)), sep = "\n  ")
          })

setMethod("show", "Ontologies",
          function(object) {
              cat("Object of class 'Ontologies' with", length(object), "entries\n")
              cat("  ", paste(head(olsPrefix(ol), n=3), collapse = ", "),
                  "...",
                  paste(tail(olsPrefix(ol), n=3), collapse = ", "), "\n")
          })


setMethod("length", "Ontologies", function(x) length(x@x))

setMethod("olsPrefix", "Ontology", function(object) object@config$preferredPrefix)
setMethod("olsPrefix", "Ontologies", function(object) sapply(object@x, olsPrefix))
setMethod("olsDesc", "Ontology", function(object) object@config$description)
setMethod("olsDesc", "Ontologies", function(object) sapply(object@x, olsDesc))
setMethod("olsTitle", "Ontology", function(object) object@config$title)
setMethod("olsTitle", "Ontologies", function(object) sapply(object@x, olsTitle))

setGeneric("ontologies", function(object) standardGeneric("ontologies"))
setMethod("ontologies", "missing", .getOntologies)

setMethod("ontologies", "Ontologies", function(object) object@x)

setMethod("[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[i])
          
setMethod("[[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[[i]])

as.data.frame.Ontolgies <- function(x)
    data.frame(Prefix = olsPrefix(x),
               Title = olsTitle(x))

ol <- ontologies()
go <- ol[["GO"]]
efo <- ol[["EFO"]]

go2 <- Ontology("go")
go3 <- Ontology("GO")

stopifnot(identical(go, go3))
stopifnot(identical(go, go3))
