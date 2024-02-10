#####################################
## httr2 utils
##' @import httr2
next_req <- function(resp, req) {
    .next <- resp_body_json(resp)[["_links"]][["next"]]$href
    if (is.null(.next))
        return(NULL)
    request(.next)
}

resp_embedded <- function(resp, what) {
    body <- resp_body_json(resp)
    body[["_embedded"]][[what]]
}


## ##' @title Constructs the query for all properties from a given ontology
## ##' @param oid A character with an ontology or an ontology
## ##' @param pagesize How many results per page to return
## ##' @return An object of class Terms
## .properties <- function(oid, pagesize = 200) {
##     ont <- Ontology(oid)
##     url <- paste(ontologyUrl(ont), "properties", sep = "/")
##     url <- paste0(url, "?&size=", pagesize)
##     makeProperties(url)
## }

## makeProperties <- function(url) {
##     x <- GET(url)
##     stop_for_status(x)
##     cx <- content(x)
##     ans <- lapply(cx[["_embedded"]][[1]], makeProperty)
##     ## -- Iterating
##     .next <- cx[["_links"]][["next"]]$href
##     while (!is.null(.next)) {
##         x <- GET(.next)
##         warn_for_status(x)
##         cx <- content(x)
##         ans <- append(ans, lapply(cx[["_embedded"]][[1]], makeProperty))
##         .next <- cx[["_links"]][["next"]][[1]]
##     }
##     names(ans) <- sapply(ans, termLabel)
##     Properties(x = ans)
## }

## ##' @title Makes a Property instance based on the response from
## ##'     /api/ontologies/{ontology}/terms/{iri}
## ##' @param x The content from the response
## ##' @return An object of class Property
## makeProperty <- function(x)
##     .Property(iri = x$iri,
##               label = x$label,
##               description = x$description,
##               annotation = x$annotation,
##               synonym = x$synonym,
##               ontology_name = x$ontology_name,
##               ontology_prefix = x$ontology_prefix,
##               ontology_iri = x$ontology_iri,
##               is_obsolete = x$is_obsolete,
##               is_defining_ontology = x$is_defining_ontology,
##               has_children = x$has_children,
##               is_root = x$is_root,
##               short_form = x$short_form,
##               obo_id = x$obo_id,
##               links = x$`_links`)
