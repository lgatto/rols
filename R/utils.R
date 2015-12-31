ontologyUrl <- function(goid)
    paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies/", goid)

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
makeOntologies <- function() {
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
    ans <- lapply(cx[["_embedded"]][[1]], makeOntology)
    names(ans) <- sapply(ans, olsPrefix)
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
##' @param goid A character with an ontology prefix
##' @param termid A character with a term id
##' @return An object of class Term
.term <- function(goid, termid) {
    url <- paste(ontologyUrl(goid), "terms", sep = "/")
    url <- paste(url, "http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F", sep = "/")
    url <- paste0(url, sub(":", "_", termid))
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    makeTerm(cx)
}

##' @title Constructs the query for all term from a given ontology
##' @param goid A character with an ontology prefix
##' @param pagesize How many results per page to return
##' @return An object of class Terms
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
