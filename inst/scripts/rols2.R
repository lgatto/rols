## Development script for rols version 2.

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
    ans <- lapply(cx[["_embedded"]][[1]], Ontology)
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

Ontology <- function(x)
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
              cat("Ontology: ", ontoTitle(object), " (", ontoPrefix(object) , ")", sep = "")
              cat("   ", strwrap(ontoDesc(object)), sep = "\n  ")
          })

setMethod("show", "Ontologies",
          function(object)
              cat("Object of class 'Ontologies' with", length(object@x), "entries\n"))

setGeneric("ontoPrefix", function(object) standardGeneric("ontoPrefix"))
setMethod("ontoPrefix", "Ontology", function(object) object@config$preferredPrefix)
setMethod("ontoPrefix", "Ontologies", function(object) sapply(object@x, ontoPrefix))
setGeneric("ontoDesc", function(object) standardGeneric("ontoDesc"))
setMethod("ontoDesc", "Ontology", function(object) object@config$description)
setMethod("ontoDesc", "Ontologies", function(object) sapply(object@x, ontoDesc))
setGeneric("ontoTitle", function(object) standardGeneric("ontoTitle"))
setMethod("ontoTitle", "Ontology", function(object) object@config$title)
setMethod("ontoTitle", "Ontologies", function(object) sapply(object@x, ontoTitle))

setGeneric("ontologies", function(object) standardGeneric("ontologies"))
setMethod("ontologies", "missing", .getOntologies)

setMethod("ontologies", "Ontologies", function(object) object@x)

setMethod("[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[i])
          
setMethod("[[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[[i]])

as.data.frame.Ontolgies <- function(x)
    data.frame(Prefix = ontoPrefix(x),
               Title = ontoTitle(x))
