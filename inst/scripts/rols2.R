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


f <- function(object) {
    prefix <- function(object)
        object@config$preferredPrefix
    p <- sapply(ol@x, prefix)
    p[duplicated(p)]
}

.Ontologies <- setClass("Ontologies", slots = c(x = "list"))

as.data.frame.Ontolgies <- function(x) 
    data.frame(Prefix = sapply(x@x, function(xx) xx@config$preferredPrefix),
               Title = sapply(x@x, function(xx) xx@config$title))


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
              x <- object@config
              cat("Ontology: ", x$title, " (", x$preferredPrefix , ")", sep = "")
              cat("   ", strwrap(x$description), sep = "\n  ")
          })


setMethod("show", "Ontologies",
          function(object)
              cat("Object of class 'Ontologies' with", length(object@x), "entries\n"))

ontologies <- function() {
    x <- GET("http://www.ebi.ac.uk/ols/beta/api/ontologies", accept_json())
    warn_for_status(x)
    cx <- content(x)    
    ans <- lapply(cx[["_embedded"]][[1]], Ontology)
    .next <- cx[["_links"]][["next"]][[1]]    
    while (!is.null(.next)) {
        x <- GET(.next)
        warn_for_status(x)
        cx <- content(x)
        ans <- append(ans, lapply(cx[["_embedded"]][[1]], Ontology))
        .next <- cx[["_links"]][["next"]][[1]]
    }
    .Ontologies(x = ans)
}
