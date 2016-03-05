setMethod("ontologyUrl", "character",
          function(object)
              paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies/", object, "/"))

setMethod("ontologyUrl", "Ontology",
          function(object) {
              nsp <- olsNamespace(object)
              paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies/", nsp, "/")
          })

## This will not always be the correct URI (see for example
## Orphaned/ORBO and https://github.com/EBISPOT/OLS/issues/35)
setMethod("ontologyUri", "missing",
          function(encode = TRUE) {
              uri <- "http://purl.obolibrary.org/obo/"
              if (encode)
                  uri <- gsub("%", "%25", URLencode(uri, TRUE))
              uri
          })

setMethod("ontologyUri", "Ontology",
          function(object, encode = TRUE, withPrefix = FALSE) {
              uri <- object@config$baseUris
              if (is.null(uri) | length(uri) == 0)
                  return(ontologyUri())
              if (length(uri) > 1) {
                  msg <- paste0("More than one URI available:\n  ",
                                paste(unlist(uri), collapse = ", "), "\n  ",
                                "Choosing the first one.\n")
                  warning(msg)
              }
              uri <- uri[[1]][1]
              if (!withPrefix)
                  uri <- sub("/[A-Za-z]+_$", "/", uri)
              if (encode)
                  uri <- gsub("%", "%25", URLencode(uri, TRUE))
              uri
          })

.termId <- function(x) x@obo_id

##' @title Makes an Ontology instance based on the response from
##'     /api/ontologies/{ontology_id}
##' @param x A valid onology prefix
##' @return An object of class Ontology
makeOntology <- function(x)
    .Ontology(loaded = x$loaded,
              updated = x$updated,
              status = x$status,
              message = x$message,
              version = x$version,
              numberOfTerms = x$numberOfTerms,
              numberOfProperties = x$numberOfProperties,
              numberOfIndividuals = x$numberOfIndividuals,
              config = x$config)


##' @title Makes an Ontologies instance based on the response from
##'     api/ontologies @return
##' @return An object of class Ontologies
##' @param pagesize A numeric indicating the number of elements per
##'     page (default in method is 150).
makeOntologies <- function(pagesize = 150) {
    x <- GET(paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies?page=0&size=",
                    pagesize))
    warn_for_status(x)
    cx <- content(x)
    if (cx$page$totalElements > pagesize) {
        pagesize <- cx$page$totalElements
        x <- GET(paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies?page=0&size=",
                        pagesize))
        warn_for_status(x)
        cx <- content(x)
    }
    ans <- lapply(cx[["_embedded"]][[1]], makeOntology)
    names(ans) <- sapply(ans, olsNamespace)
    .Ontologies(x = ans)
}

##' @title Makes a Term instance based on the response from
##'     /api/ontologies/{ontology}/terms/{iri}
##' @param x The content from the response
##' @return An object of class Term
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


##' @title Constructs the query for a single term from a given
##'     ontology
##' @param oid A character with an ontology or an ontology
##' @param termid A character with a term id
##' @return An object of class Term
.term <- function(oid, termid) {
    ont <- Ontology(oid)
    url <- paste0(ontologyUrl(ont), "terms", "/")
    uri <- URLencode(ontologyUri(ont), TRUE)
    url <- paste0(url, uri, sub(":", "_", termid))
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    makeTerm(cx)
}

##' @title Constructs the query for all term from a given ontology
##' @param oid A character with an ontology or an ontology
##' @param pagesize How many results per page to return
##' @return An object of class Terms
.terms <- function(oid, pagesize = 1000) {
    ont <- Ontology(oid)
    url <- paste(ontologyUrl(ont), "terms", sep = "/")
    url <- paste0(url, "?&size=", pagesize)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    ## -- Iterating
    .next <- cx[["_links"]][["next"]]$href
    pb <- progress_bar$new(total = cx[["page"]][["totalPages"]])
    pb$tick()
    while (!is.null(.next)) {
        pb$tick()
        x <- GET(.next)
        warn_for_status(x)
        cx <- content(x)
        ans <- append(ans, lapply(cx[["_embedded"]][[1]], makeTerm))
        .next <- cx[["_links"]][["next"]][[1]]
    }
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

##' @title Constructs the query for all properties from a given ontology
##' @param oid A character with an ontology or an ontology
##' @param pagesize How many results per page to return
##' @return An object of class Terms
.properties <- function(oid, pagesize = 200) {
    ont <- Ontology(oid)
    url <- paste(ontologyUrl(ont), "properties", sep = "/")
    url <- paste0(url, "?&size=", pagesize)
    makeProperties(url)
}

makeProperties <- function(url) {
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeProperty)
    ## -- Iterating
    .next <- cx[["_links"]][["next"]]$href
    while (!is.null(.next)) {
        x <- GET(.next)
        warn_for_status(x)
        cx <- content(x)
        ans <- append(ans, lapply(cx[["_embedded"]][[1]], makeProperty))
        .next <- cx[["_links"]][["next"]][[1]]
    }
    names(ans) <- sapply(ans, termLabel)
    Properties(x = ans)
}

##' @title Makes a Property instance based on the response from
##'     /api/ontologies/{ontology}/terms/{iri}
##' @param x The content from the response
##' @return An object of class Property
makeProperty <- function(x)
    .Property(iri = x$iri,
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


## see https://github.com/EBISPOT/OLS/issues/36
getPropertyLinks <- function(trm) {
    termlinks <- c("self", "parents", "ancestors",
                   "children", "descendants",
                   "part_of","derives_from")
    graphlinks <- c("jstree", "graph")
    nms <- names(trm@links)
    p <- !nms %in% c(termlinks, graphlinks)
    unlist(trm@links[p])
}
