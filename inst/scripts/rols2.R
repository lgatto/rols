## Development script for rols version 2.

## Release plan
##  Update release package on the 1st of March, so that the release
##  version continues to work using the archive URL until the next
##  release in April. Commit a devel version, that uses the REST api,
##  before release.


## http://www.ebi.ac.uk/ols/beta/docs/api
## http://www.ebi.ac.uk/ols/beta/roadmap.html

## "ontologies"
## ontologyLoadDate -> olsLoaded
##                  -> olsUpdated
## olsVersion -> olsVersion
## allIds -> terms
## "isIdObsolete" -> isObsolete
## "rootId"       -> olsRoot

## CVParams
## "as.character.CVParam"
## "charIsCVParam"

## Dropping
## .rols environment
## "as.character.Map"  "key"  "CVParam"
## ontologyNames -> see ontologies
## "as.character.mapItem"

##  TODO
## > ls("package:rols")
## "olsQuery"             
## "term"                  "termMetadata"
##  "value"                "termXrefs"

## term graph and jstree for a given term

## Properties and individuals
## Search/select 


library("httr")
library("progress")
library("jsonlite")

setGeneric("olsPrefix", function(object, ...) standardGeneric("olsPrefix"))
setGeneric("olsDesc", function(object, ...) standardGeneric("olsDesc"))
setGeneric("olsTitle", function(object, ...) standardGeneric("olsTitle"))
setGeneric("olsTitle", function(object, ...) standardGeneric("olsTitle"))
setGeneric("olsRoot", function(object, ...) standardGeneric("olsRoot"))
setGeneric("terms", function(object, ...) standardGeneric("terms"))
setGeneric("term", function(object, id, ...) standardGeneric("term"))
setGeneric("termId", function(object, ...) standardGeneric("termId"))
setGeneric("ontologies", function(object) standardGeneric("ontologies"))
setGeneric("olsQuery", function(object,  ...) standardGeneric("olsQuery"))

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


setMethod("olsRoot", "character",
          function(object) {
              url <- ontologyUrl(object)
              url <- paste0(url, "/terms/roots")
              x <- GET(url)
              stop_for_status(x)
              cx <- content(x)
              ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
              names(ans) <- sapply(ans, termId)
              Terms(x = ans)
          })

setMethod("olsRoot", "Ontology", function(object) olsRoot(olsPrefix(object)))

olsLoaded <- function(x) substr(x@loaded, 1, 10)
olsUpdated <- function(x) substr(x@updated, 1, 10)
olsVersion <- function(x) x@config$version
isObsolete <- function(x) x@is_obsolete
isRoot <- function(x) x@is_root

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





.termId <- function(x) x@obo_id

setMethod("termId", "Term", function(object) .termId(object))
setMethod("termId", "Terms",
          function(object) sapply(object@x, .termId))


setMethod("show", "Term",
          function(object) {
              ids <- .termId(object)
              cat("A Term from the", object@ontology_prefix, "ontology:", ids, "\n")
              cat(" Label: ", object@label,"\n  ", sep = "")
              desc <- object@description
              if (is.null(desc)) cat("No description\n")
              else for (i in 1:seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })

setMethod("length", "Terms", function(x) length(x@x))

setMethod("show", "Terms",
          function(object) {
              cat("Object of class 'Terms' with", length(object), "entries\n")
              if (length(object) > 4)
                  cat("  ", paste(head(termId(object), n=2), collapse = ", "),
                      "...",
                      paste(tail(termId(object), n=2), collapse = ", "), "\n")
              else
                  cat(paste(termId(object)[1:4], collapse = ", "), "\n")
          })


setMethod("[", "Terms",
          function(x, i, j="missing", drop="missing") Terms(x = x@x[i]))
          
setMethod("[[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[[i]])

makeTerm <- function(x)
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
              cat("   Loaded:", olsLoaded(object),
                  "Updated:", olsUpdated(object),
                  "Version:", olsVersion(object), "\n")
              cat("  ", object@numberOfTerms, "terms ", 
                  object@numberOfProperties, "properties ",
                  object@numberOfIndividuals, "individuals\n")
          })

setMethod("show", "Ontologies",
          function(object) {
              cat("Object of class 'Ontologies' with", length(object), "entries\n")
              if (length(object) > 4)
                  cat("  ", paste(head(olsPrefix(object), n=3), collapse = ", "),
                      "...",
                      paste(tail(olsPrefix(object), n=3), collapse = ", "), "\n")
              else
                  cat(paste(olsPrefix(object)[1:4], collapse = ", "), "\n")
          })


setMethod("length", "Ontologies", function(x) length(x@x))

setMethod("olsPrefix", "Ontology", function(object) object@config$preferredPrefix)
setMethod("olsPrefix", "Ontologies", function(object) sapply(object@x, olsPrefix))
setMethod("olsDesc", "Ontology", function(object) object@config$description)
setMethod("olsDesc", "Ontologies", function(object) sapply(object@x, olsDesc))
setMethod("olsTitle", "Ontology", function(object) object@config$title)
setMethod("olsTitle", "Ontologies", function(object) sapply(object@x, olsTitle))

setMethod("ontologies", "missing", .getOntologies)

setMethod("ontologies", "Ontologies", function(object) object@x)

setMethod("[", "Ontologies",
          function(x, i, j="missing", drop="missing") Ontologies(x = x@x[i]))
          
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
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    ## -- Iterating
    .next <- cx[["_links"]][["next"]]$href
    pb <- progress_bar$new(total = cx[["page"]][["totalPages"]])
    while (!is.null(.next)) {
        pb$tick()
        x <- GET(.next)
        warn_for_status(x)
        cx <- content(x)
        ans <- append(ans, lapply(cx[["_embedded"]][[1]], makeTerm))
        .next <- cx[["_links"]][["next"]][[1]]
    }
    cat("\n")
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

setMethod("terms", "character", function(object, ...) .terms(object, ...))
setMethod("terms", "Ontology", function(object, ...) .terms(olsPrefix(object), ...))

.term <- function(goid, termid) {
    url <- paste(ontologyUrl(goid), "terms", sep = "/")
    url <- paste(url, "http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F", sep = "/")
    url <- paste0(url, sub(":", "_", termid))
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    makeTerm(cx)
}

children <- function(id) { ## a Term
    x <- GET(id@links$children[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

parents <- function(id) { ## a Term
    x <- GET(id@links$parents[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

ancestors <- function(id) { ## a Term
    x <- GET(id@links$ancestors[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

descendants <- function(id) { ## a Term
    x <- GET(id@links$descendants[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}


setMethod("term", c("character", "character"),
          function(object, id, ...) .term(object, id, ...))
setMethod("term", c("Ontology", "character"),
          function(object, id,...) .term(olsPrefix(object), id, ...))

setMethod("lapply", "Ontologies",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
setMethod("lapply", "Terms",
          function(X, FUN, ...) lapply(X@x, FUN, ...))


.OlsSearch <- setClass("OlsSearch",
                       slots = c(q = "character",
                                 ontology = "character",
                                 type = "character",
                                 slim = "character",
                                 fieldList = "character",
                                 queryFields = "character",
                                 exact = "logical",
                                 groupField = "logical",
                                 obsoletes = "logical",
                                 local = "character",
                                 childrenOf = "character",
                                 rows = "integer", 
                                 start = "integer",
                                 url = "character",
                                 numFound = "integer",
                                 response = "data.frame"))

setMethod("show", "OlsSearch",
          function(object) {
              cat("Object of class 'OlsSearch':\n")
              if (object@ontology[1] != "") {
                  if (length(object@ontology) == 1)
                      cat("  ontolgy:", object@ontology, "\n")
                  else
                      cat("  ontolgies:",
                          paste(object@ontology, collapse = ", "), "\n")
              }
              cat("  query:", object@q, "\n")
              cat("  requested: ", object@rows, " (out of ",
                  object@numFound, ")\n", sep ="")
              cat("  response(s):", nrow(object@response), "\n")              
          })

OlsSearch <- function(q,
                      ontology = "",
                      type = "",
                      slim = "",
                      fieldList = "",
                      queryFields = "",
                      exact = FALSE,
                      groupField = FALSE,
                      obsoletes = FALSE,
                      local = "",
                      childrenOf = "",
                      rows,
                      start = 0L) {
    if (missing(q))
        stop("You must supply a query.")
    .args <- as.list(match.call())[-1]
    if (missing(rows)) 
        .args[["rows"]] <- rows <- 20L
    ## Create search URL and instantiate OlsSearch object
    params <- c()
    for (i in seq_along(.args)) {
        nm <- names(.args)[i]
        arg <- eval(.args[[i]])
        if (nm == "ontology")
            arg <- tolower(arg)
        if (length(arg) > 1)
            arg <- paste(arg, collapse = ",")
        params <- append(params, paste(nm, arg, sep = "="))
    }
    url <- paste0("http://www.ebi.ac.uk/ols/beta/api/search?",
                  paste(params, collapse = "&"))
    ## Make actual query, with rows = 1 to get the total number of
    ## results found
    url0 <- sub("rows=[0-9]+", "rows=1", url)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    txt <- rawToChar(cx)
    ans <- jsonlite::fromJSON(txt)
    numFound <- ans[["response"]][["numFound"]]
    response <- data.frame()
    .OlsSearch(q = q, ontology = ontology, slim = slim,
               fieldList = fieldList, queryFields = queryFields,
               exact = exact, groupField = groupField,
               obsoletes = obsoletes, local = local,
               childrenOf = childrenOf, rows = as.integer(rows),
               start = as.integer(start), url = url,
               numFound = as.integer(numFound),
               response = response)
}


olsRows <- function(x) x@rows

"olsRows<-" <- function(x, value) {
    x@rows <- as.integer(value)
    x
}

allRows <- function(x) {
    x@rows <- x@numFound
    x@url <- sub("rows=[0-9]+", paste0("rows=", x@numFound), x@url)
    x
}


as.data.frame.OlsSearch <-
    function(x) x@response

setAs(from = "OlsSearch", to = "data.frame",
      function(from) from@response)

setAs(from = "OlsSearch", to = "Terms",
      function(from) {
          x <- apply(from@response, 1,
                     function(x) term(x[["ontology_prefix"]],
                                      x[["obo_id"]]))
          Terms(x = x)
      })


olsSearch <- function(object, all = FALSE) {
    if (all)
        x <- allRows(x)
    x <- GET(object@url)
    stop_for_status(x)
    cx <- content(x)
    txt <- rawToChar(cx)
    ans <- jsonlite::fromJSON(txt)
    object@response <- ans[["response"]][["docs"]]
    object
}


## EXAMPLES

## Get all ontolgies
ol <- ontologies()
ol

## Summarise ontologies
(go <- ol[["GO"]])
(efo <- ol[["EFO"]])

## Directly initialise one ontology
go1 <- Ontology("go")
GO <- Ontology("GO")

stopifnot(identical(go, GO))
stopifnot(identical(go, go1))

## Queries

## (all) terms
gotrms <- terms(go, pagesize = 10000)

## (one) term

(trm <- gotrms[[1]])
gotrms[1:3]
gotrms[["GO:0032801"]]

term("GO", "GO:0032801")
term(go, "GO:0032801")

isObsolete(gotrms[["GO:0030533"]])
isObsolete(gotrms[["GO:0005563"]])

isRoot(gotrms[["GO:0030533"]])

i <- which(unlist(lapply(gotrms, function(x) isRoot(x) & !isObsolete(x))))
for (ii in i)
    show(gotrms[[ii]])

olsRoot(go)
identical(olsRoot("GO"), olsRoot(go))

parents(trm)
ancestors(trm)
children(trm)
descendants(trm)

## searching


OlsSearch(q = "trans-golgi")
OlsSearch(q = "cell")
OlsSearch(q = "cell", exact = TRUE)
OlsSearch(q = "cell", exact = TRUE, ontology = "go")
OlsSearch(q = "cell", exact = TRUE, ontology = "GO")
OlsSearch(q = "plasma,membrane", ontology = "go")

res <- OlsSearch(q = "trans-golgi", ontology = "go", rows = 5)
res
res <- olsSearch(res)
res
as(res, "data.frame")
as(res, "Terms")
