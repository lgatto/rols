##' @title Term Properties
##'
##' @aliases olsProperty
##' @aliases olsProperties olsProperties,character olsProperties,olsOntology
##' @aliases olsProperties,olsTerm olsProperties,olsTerms
##' @aliases olsProperties,length
##'
##' @name olsProperties
##'
##' @description
##'
##' Properties (relationships) between terms can be queries for
##' complete [olsOntology()] objects and [olsTerm()]/[olsTerms()]
##' instances, and the results are stored as objects of class
##' `olsProperty` or `olsProperties`.
##'
##' @references
##'
##' - OLS3 API (the OLS4 API should function identically to the OLS3):
##'   <http://www.ebi.ac.uk/ols4/ols3help>
##'
##' - REST API for OLS: <https://www.ebi.ac.uk/ols4/swagger-ui/index.html>
##'
##' @examples
##'
##' ## Term properties
##' trm <- olsTerm("uberon", "UBERON:0002107")
##' trm
##' olsProperties(trm)
##'
##' ## Ontology properties
##' olsProperties('ado')
NULL

##########################################
## Classes
.olsProperty <- setClass("olsProperty",
                      contains = "olsTerm")
.olsProperties <- setClass("olsProperties", contains = "olsTerms")


##########################################
## Constructors
##' @export
##' @rdname olsProperties
##'
##' @param object First input object.
setMethod("olsProperties", "olsOntology",
          function(object)
              olsProperties(olsNamespace(object)))

##' @export
##' @rdname olsProperties
setMethod("olsProperties", "character",
          function(object) {
              url <-
                  paste0("http://www.ebi.ac.uk/ols4/api/ontologies/",
                         object, "/properties")
              x <- lapply(ols_requests(url, what = "properties"),
                          propertyFromJson)
              .olsProperties(x = x)
          })

##' @export
##' @rdname olsProperties
setMethod("olsProperties", "olsTerm",
          function(object) {
              urls <- getOlsPropertyLinks(object)
              if (length(urls) == 0) {
                  message("No properties for term ", termId(object))
                  return(NULL)
              }
              .properiesFromJson <- function(url)
                  lapply(ols_requests(url, what = "terms"),
                         propertyFromJson)
              ans <- unlist(lapply(urls, .properiesFromJson))
              .olsProperties(x = ans)
          })
##' @export
##' @rdname olsProperties
setMethod("olsProperties", "olsTerms",
          function(object) {
              ans <- lapply(object@x, olsProperties)
              ans <- unlist(lapply(ans, "slot", "x"),
                            use.names = FALSE)
              .olsProperties(x = ans)
          })

##########################################
## show methods
##' @export
##' @rdname olsProperties
setMethod("show", "olsProperty",
          function(object) {
              ids <- termId(object)
              cat("A olsProperty from the", termPrefix(object),
                  "ontology:", ids, "\n")
              cat(" Label: ", termLabel(object),"\n", sep = "")
          })

##' @export
##' @rdname olsProperties
setMethod("show", "olsProperties",
          function(object) {
              cat("Object of class 'olsProperties' with", length(object),
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
##' @rdname olsProperties
##'
##' @param x A `olsProperties` object.
setMethod("length", "olsProperties", function(x) length(x@x))

#########################################
## Helper functions
propertyFromJson <- function(x)
    .olsProperty(iri = x[["iri"]],
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
getOlsPropertyLinks <- function(trm) {
    termlinks <- c("self", "parents", "ancestors",
                   "children", "descendants",
                   "part_of","derives_from")
    graphlinks <- c("jstree", "graph")
    nms <- names(termLinks(trm))
    p <- !nms %in% c(termlinks, graphlinks)
    termLinks(trm)[p]
}

## Try also from URL
## curl 'http://www.ebi.ac.uk/ols/beta/api/ontologies/go/properties/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FBFO_0000051' -i -H 'Accept: application/json'
