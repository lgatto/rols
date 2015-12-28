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
library("progress")

setGeneric("olsPrefix", function(object, ...) standardGeneric("olsPrefix"))
setGeneric("olsDesc", function(object, ...) standardGeneric("olsDesc"))
setGeneric("olsTitle", function(object, ...) standardGeneric("olsTitle"))
setGeneric("olsTitle", function(object, ...) standardGeneric("olsTitle"))
setGeneric("terms", function(object, ...) standardGeneric("terms"))

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
    Ontologies(x = ans)
}

setClassUnion("NullOrChar", c("NULL", "character"))
setClassUnion("NullOrList", c("NULL", "list"))

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

Ontologies <- setClass("Ontologies", slots = c(x = "list"))

.Term <- setClass("Term",
                  slots = c(iri = "character",
                            label = "character",
                            description = "NullOrList",
                            annotation = "list",
                            synonym = "NullOrList",
                            ontology_name = "character",
                            ontology_prefix = "character",
                            ontology_iri = "character",
                            is_obsolete = "logical",
                            is_defining_ontology = "logical",
                            has_children = "logical",
                            is_root = "logical",
                            short_form = "character",
                            obo_id = "NullOrChar",
                            links = "list"))

Terms <- setClass("Terms", slots = c(x = "list"))

.termID <- function(x, simplify = TRUE) {
    idlist <- x@annotation$id
    if (length(idlist) == 0)
        idlist <- NA_character_
    if (simplify & length(idlist) == 1)
        idlist <- idlist[[1]]
    idlist    
}

setMethod("show", "Term",
          function(object) {
              ids <- paste(unlist(.termID(object)), collapse = ", ")
              cat("A Term from the", object@ontology_prefix, "ontology:", ids, "\n")
              cat(" Label: ", object@label,"\n  ", sep = "")
              desc <- object@description
              if (is.null(desc)) cat("No description\n")
              else for (i in 1:seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })

setMethod("length", "Terms", function(x) length(x@x))

setMethod("show", "Terms",
          function(object)
              cat("Object of class 'Terms' with", length(object), "entries\n"))

setMethod("[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[i])
          
setMethod("[[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[[i]])

.makeTerm <- function(x)
    .Term(iri = x$iri,
          label = x$label,
          description = x$description,
          annotation = x$annotation,
          synonym = x$synonym,
          ontology_name = x$ontology_name,
          ontology_prefix = x$ontology_prefix,
          ontology_iri = x$ontology_iri,
          is_obsolete = x$is_obsolete,
          is_defining_ontology = x$is_defining_ontology,
          has_children = x$has_children,
          is_root = x$is_root,
          short_form = x$short_form,
          obo_id = x$obo_id,
          links = x$`_links`)


ontologyUrl <- function(goid)
    paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies/", goid)

Ontology <- function(x) {
    url <- ontologyUrl(x)
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
              cat("Ontology: ", olsTitle(object),
                  " (", olsPrefix(object) , ")", sep = "")
              cat("  ", strwrap(olsDesc(object)), sep = "\n  ")
              cat("   Loaded:", substr(object@loaded, 1, 10),
                  "Updated:", substr(go@updated, 1, 10),
                  "Version:", object@config$version, "\n")
              cat("  ", object@numberOfTerms, "terms ", 
                  object@numberOfProperties, "properties ",
                  object@numberOfIndividuals, "individuals\n")
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

setAs("Ontologies", "data.frame",
      function(from) as.data.frame.Ontologies(from))

as.data.frame.Ontologies <- function(x)
    data.frame(Prefix = olsPrefix(x),
               Title = olsTitle(x))

.terms <- function(goid, pagesize = 200) {
    url <- paste(ontologyUrl(goid), "terms", sep = "/")
    url <- paste0(url, "?&size=", pagesize)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], .makeTerm)
    ## -- Iterating
    .next <- cx[["_links"]][["next"]]$href
    pb <- progress_bar$new(total = cx[["page"]][["totalPages"]])
    while (!is.null(.next)) {
        pb$tick()
        x <- GET(.next)
        warn_for_status(x)
        cx <- content(x)
        ans <- append(ans, lapply(cx[["_embedded"]][[1]], .makeTerm))
        .next <- cx[["_links"]][["next"]][[1]]
    }
    cat("\n")
    Terms(x = ans)
}

setMethod("terms", "character", function(object, ...) .terms(object, ...))
setMethod("terms", "Ontology", function(object, ...) .terms(olsPrefix(object), ...))

# EXAMPLES

## get all ontolgies
ol <- ontologies()
ol

## summarise ontolofies
(go <- ol[["GO"]])
(efo <- ol[["EFO"]])

## or directly initialise one ontology
go1 <- Ontology("go")
GO <- Ontology("GO")

stopifnot(identical(go, GO))
stopifnot(identical(go, go1))



## Queries
## (all) terms

gotrms <- terms(go, pagesize = 1000)

## (one) term
## Properties and individuals
## Search/select
