## .Property <- setClass("Property",
##                       contains = "Term")
## Properties <- setClass("Properties", contains = "Terms")


## ##########################################
## ## Constructors
## setMethod("properties", "character",
##           function(object, ...) .properties(object, ...))
## setMethod("properties", "Ontology",
##           function(object, ...) .properties(olsNamespace(object), ...))

## setMethod("properties", "Term",
##           function(object, ...) {
##               urls <- getPropertyLinks(object)
##               if (length(urls) == 0) {
##                   message("No properties for term ", termId(object))
##                   return(NULL)
##               }
##               ans <- lapply(urls, makeProperties)
##               ans <- unlist(lapply(ans, "slot", "x"))
##               names(ans) <- sub("\\.href\\.", "/", names(ans))
##               Properties(x = ans)
##           })

## setMethod("properties", "Terms",
##           function(object, ...) {
##               lapply(object@x, properties, ...)
##           })

## ##########################################
## ## show methods

## setMethod("show", "Property",
##           function(object) {
##               ids <- termId(object)
##               cat("A Property from the", termPrefix(object), "ontology:", ids, "\n")
##               cat(" Label: ", termLabel(object),"\n", sep = "")
##           })


## setMethod("show", "Properties",
##           function(object) {
##               cat("Object of class 'Properties' with", length(object),
##                   ifelse(length(object) > 1,
##                          "entries\n",
##                          "entry\n"))
##               onts <- unique(termPrefix(object))
##               if (length(onts) == 1)
##                   cat(" From the", onts, "ontology\n")
##               else if (length(onts) < 6)
##                   cat(" From the", paste(onts, collapse = ", "), "ontologies\n")
##               else cat(" From ", length(onts), "ontologies\n")
##               n <- length(object)
##               if (n > 4)
##                   cat(" ", paste(head(termLabel(object), n=2), collapse = ", "),
##                       "...",
##                       paste(tail(termLabel(object), n=2), collapse = ", "), "\n")
##               else
##                   cat(paste(termLabel(object)[1:n], collapse = ", "), "\n")
##           })

## ##########################################
## ## Data manipulation

## setMethod("length", "Properties", function(x) length(x@x))

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
