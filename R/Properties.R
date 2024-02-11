##' @title Term Properties
##'
##' @name Properties
##'
##' @description
##'
##' Properties (relationships) between terms can be queries for
##' complete [Ontology()] objects and [Term()]/[Terms()] instances,
##' and the results are stored as objects of class `Property` or
##' `Properties`.
##'
##' @examples
##'
##' ## Term properties
##' trm <- Term("uberon", "UBERON:0002107")
##' trm
##' Properties(trm)
##'
##' ## Ontology properties
##' Properties('ado')
NULL

##########################################
## Classes
.Property <- setClass("Property",
                      contains = "Term")
.Properties <- setClass("Properties", contains = "Terms")


##########################################
## Constructors
##' @export
setMethod("Properties", "Ontology",
          function(object, ...)
              Properties(olsNamespace(object)))

##' @export
setMethod("Properties", "character",
          function(object, ...) {
              url <-
                  paste0("http://www.ebi.ac.uk/ols4/api/ontologies/",
                         object, "/properties")
              x <- lapply(ols_requests(url, what = "properties"),
                          propertyFromJson)
              .Properties(x = x)
          })

##' @export
setMethod("Properties", "Term",
          function(object, ...) {
              urls <- getPropertyLinks(object)
              if (length(urls) == 0) {
                  message("No properties for term ", termId(object))
                  return(NULL)
              }
              .properiesFromJson <- function(url)
                  lapply(ols_requests(url, what = "terms"),
                         propertyFromJson)
              ans <- unlist(lapply(urls, .properiesFromJson))
              .Properties(x = ans)
          })
##' @export
setMethod("Properties", "Terms",
          function(object, ...) {
              ans <- lapply(object@x, Properties, ...)
              ans <- unlist(lapply(ans, "slot", "x"),
                            use.names = FALSE)
              .Properties(x = ans)
          })

##########################################
## show methods
##' @export
setMethod("show", "Property",
          function(object) {
              ids <- termId(object)
              cat("A Property from the", termPrefix(object),
                  "ontology:", ids, "\n")
              cat(" Label: ", termLabel(object),"\n", sep = "")
          })

##' @export
setMethod("show", "Properties",
          function(object) {
              cat("Object of class 'Properties' with", length(object),
                  ifelse(length(object) > 1,
                         "entries\n",
                         "entry\n"))
              onts <- unique(termPrefix(object))
              if (length(onts) == 1)
                  cat(" From the", onts, "ontology\n")
              else if (length(onts) < 6)
                  cat(" From the", paste(onts, collapse = ", "), "ontologies\n")
              else cat(" From ", length(onts), "ontologies\n")
              n <- length(object)
              if (n > 4)
                  cat(" ", paste(head(termLabel(object), n=2), collapse = ", "),
                      "...",
                      paste(tail(termLabel(object), n=2), collapse = ", "), "\n")
              else
                  cat(paste(termLabel(object)[1:n], collapse = ", "), "\n")
          })

##########################################
## Data manipulation
##' @export
setMethod("length", "Properties", function(x) length(x@x))

#########################################
## Helper functions
propertyFromJson <- function(x)
    .Property(iri = x[["iri"]],
              lang = x[["lang"]],
              description = x[["description"]],
              synonyms = x[["synonyms"]],
              annotation = x[["annotation"]],
              label = x[["label"]],
              ontology_name = x[["ontology_name"]],
              ontology_prefix = x[["ontology_prefix"]],
              ontology_iri = x[["ontology_iri"]],
              is_obsolete = x[["is_obsolete"]],
              term_replaced_by = x[["term_replaced_by"]],
              is_defining_ontology = x[["is_defining_ontology"]],
              has_children = x[["has_children"]],
              is_root = x[["is_root"]],
              short_form = x[["short_form"]],
              obo_id = x[["obo_id"]],
              in_subset = x[["in_subset"]],
              obo_definition_citation = x[["obo_definition_citation"]],
              obo_xref = x[["obo_xref"]],
              obo_synonym = x[["obo_synonym"]],
              is_preferred_root = x[["is_preferred_root"]],
              links = x[["_links"]])


## see https://github.com/EBISPOT/OLS/issues/36
getPropertyLinks <- function(trm) {
    termlinks <- c("self", "parents", "ancestors",
                   "children", "descendants",
                   "part_of","derives_from")
    graphlinks <- c("jstree", "graph")
    nms <- names(termLinks(trm))
    p <- !nms %in% c(termlinks, graphlinks)
    termLinks(trm)[p]
}
