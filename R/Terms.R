##' @title Ontology Terms
##'
##' @aliases olsTerm olsTerms olsTerms,character olsTerms,olsOntology
##' @aliases termLinks termLinks,olsTerm
##' @aliases children parents ancestors descendants
##' @aliases termSynonym termSynonym,olsTerm termSynonym,olsTerms
##' @aliases isObsolete isObsolete,olsTerm isObsolete,olsTerms
##' @aliases isRoot isRoot,olsTerm isRoot,olsTerms
##' @aliases termLabel termLabel,olsTerm termLabel,olsTerms
##' @aliases termId termId,olsTerm termId,olsTerms
##' @aliases termPrefix termPrefix,olsTerm termPrefix,olsTerms
##' @aliases termDesc termDesc,olsTerm termDesc,olsTerms
##' @aliases termOntology termOntology,olsTerm termOntology,olsTerms
##' @aliases termNamespace termNamespace,olsTerm termNamespace,olsTerms
##'
##' @description
##'
##' The `olsTerm` class describes an ontology term. A set of terms are
##' instantiated as a `olsTerms` class.
##'
##' @section Contructors:
##'
##' Objects can be created using the `olsTerm()` and `olsTerms()`
##' constructers. The latter is used with an object of class
##' `olsOntology` or a `character` describing a valid ontology prefix
##' to download and instantiate all terms of an ontology of
##' interest. The former takes an `olsOntology` object (or an ontology
##' prefix) and a term identifer to instantiate that specific term.
##'
##' For any given `olsTerm` object, the `children`, `parents`,
##' `ancestors` and `descendants` terms can be generated with the
##' `children()`, `parents()`, `ancestor()` and `descendants()`
##' function. `olsTerms` instances can be subset with `[` and `[[` and
##' iterated over with `lapply`.
##'
##' @section Accessors:
##'
##' - `isObsolete(object = "olsTerm")` returns a `TRUE` if the term is
##'   obsolete, `FALSE` otherwise. Also works on `olsTerms` instances.
##'
##' - `isRoot(object = "olsTerm")` returns a `TRUE` if the term is a root
##'   term, `FALSE` otherwise. Also works on `olsTerms` instances.
##'
##' - `termDesc(object = "olsTerm")` returns a `character` with the
##'   term's description. Also works on `olsTerms` instances.
##'
##' - `termId(object = "olsTerm")` returns a `character` with the term's
##'   identifier. Also works on `olsTerms` instances.
##'
##' - `termLabel(object = "olsTerm")` returns a `character` with the
##'   term's label. Also works on `olsTerms` instances.
##'
##' - `termNamespace(object = "olsTerm")` returns a `character` with the
##'   term's namespace. Also works on `olsTerms` instances.
##'
##' - `termOntology(object = "olsTerm")` returns a `character` with the
##'   term's ontology (where it was retrieved from). Also works on
##'   `olsTerms` instances.
##'
##' - `termPrefix(object = "olsTerm")` returns a `character` with the
##'   term's (ontology) prefix (where it was retrieved from). Also
##'   works on `olsTerms` instances.
##'
##' - `termSynonym(object = "olsTerm")` returns a `character` with the
##'   term's synpnym(s). Also works on `olsTerms` instances.
##'
##' - `termLinks(object = "olsTerm")` returns a named `character` with
##'   hyperlink to/from the term.
##'
##' @section Related terms:
##'
##' - `children(object = "olsTerm")` returns a new `olsTerms` instance with
##'    the `object`'s children or `NULL` if there are no children.
##'
##' - `parents(object = "olsTerm")` returns a new `olsTerms` instance with
##'   the `object`'s parents or `NULL` if there are no parents.
##'
##' - `ancestors(object = "olsTerm")` returns a new `olsTerms` instance with
##'   the `object`'s ancestors or `NULL` if there are no ancestors.
##'
##' - `descendants(object = "olsTerm")` returns a new `olsTerms` instance
##'   with the `object`'s descendants or `NULL` if there are no
##'   descendants.
##'
##' @section Coercion:
##'
##' - `as(x, "data.fram")` coerces a `olsTerm` or `olsTerms` instance into a
##'   `data.frame` of length 1 (for the former) or length `length(x)`
##'   for the latter. The result will contain the following columns:
##'   id, label, description of the term(s), their ontology, whether
##'   they are obsolete, have children or are root node, the first
##'   synonym only, their iri and whether they are defining the
##'   ontology. Any missing value will be reported as `NA`.
##'
##' @references
##'
##' - OLS3 API (the OLS4 API should function identically to the OLS3):
##'   <http://www.ebi.ac.uk/ols4/ols3help>
##'
##' - REST API for OLS: <https://www.ebi.ac.uk/ols4/swagger-ui/index.html>
##'
##' @name olsTerms
##'
##' @author Laurent Gatto
##'
##' @examples
##'
##' ## Alzheimer's Disease Ontology (ADO)
##' (adoterms <- olsTerms('ado'))
##'
##' ## Focus on squamous epithelium
##' (trm <- adoterms[["UBERON:0006914"]])
##'
##' ## Accessors
##' termLabel(trm)
##' head(termLabel(adoterms))
##' termId(trm)
##' termDesc(trm)
##' termOntology(trm)
##' termNamespace(trm)
##' termSynonym(trm) ## none
##'
##' ## Related terms
##' children(trm)
##' descendants(trm) ## includes child
##'
##' parents(trm)
##' ancestors(trm) ## includes parent
##'
##' ## A single term from an ontology
##' olsTerm("ado", "ADO:0000090")
NULL

############################################################
## A single term
.olsTerm <- setClass("olsTerm",
                  slots = c(iri = "character",
                            lang = "character",
                            description = "NullOrList",
                            synonyms = "NullOrList",
                            annotation = "NullOrList",
                            label = "character",
                            ontology_name = "character",
                            ontology_prefix = "character",
                            ontology_iri = "character",
                            is_obsolete = "logical",
                            term_replaced_by = "NullOrChar",
                            is_defining_ontology = "logical",
                            has_children = "logical",
                            is_root = "logical",
                            short_form = "NullOrChar",
                            obo_id = "NullOrChar",
                            in_subset = "NullOrList",
                            obo_definition_citation = "NullOrList",
                            obo_xref  = "NullOrList",
                            obo_synonym = "NullOrList",
                            is_preferred_root = "NullOrLogical",
                            links = "list"))

############################################################
## A list of terms
.olsTerms <- setClass("olsTerms", slots = c(x = "list"))

##########################################
## Constructors

##' @export
##' @rdname olsTerms
##'
##' @param object generally an instance of class `olsTerms` or `olsTerm`. In
##'     some cases, an ontology identifier is applicable.
##'
##' @param pagesize `numeric(1)`, converted to an integer, defining
##'     the response page size. Default is 1000.
##'
##' @param obsolete `NULL` or `logical(1)` defining whether obsolete
##'     terms (`TRUE`), current terms (`FALSE`) or all (`NULL`,
##'     default) should be returned.
setMethod("olsTerms", "character", ## ontologyId
          function(object, pagesize = 1000, obsolete = NULL)
              makeOlsTerms(object, pagesize, obsolete))

##' @export
##' @rdname olsTerms
setMethod("olsTerms", "olsOntology",
          function(object, pagesize = 1000, obsolete = NULL)
              makeOlsTerms(object, pagesize, obsolete))

##' @export
##' @rdname olsTerms
##'
##' @param id `character(1)` with the term's identifier.
setMethod("olsTerm", "character",
          function(object, id) olsTerm(olsOntology(object), id))

##' @export
##' @rdname olsTerms
setMethod("olsTerm", "olsOntology",
          function(object, id) {
              ## See https://github.com/EBISPOT/ols4/issues/621
              url <- paste0(
                  "https://www.ebi.ac.uk/ols4/api/ontologies/",
                  olsNamespace(object),
                  "/terms/")
              loc <- object@config$fileLocation
              if (grepl("ebi.ac.uk", loc)) {
                  uri <- sub("/[a-zA-z]+\\.owl$", "", loc)
              }
              else if (grepl("purl.obolibrary.org", loc)) {
                  uri <- "http://purl.obolibrary.org/obo"
              } else stop("Unknown fileLocation")
              uri <- paste0(uri, "/", sub(":", "_", id))
              uri <- gsub("%", "%25", URLencode(uri, TRUE))
              url <- paste0(url, uri)
              request(url) |>
                  req_perform() |>
                  resp_body_json() |>
                  termFromJson()
          })


##' @export
##' @rdname olsTerms
children <- function(object) {
    stopifnot(inherits(object, "olsTerm"))
    if (!object@has_children)
        return(NULL)
    url <- termLinks(object)[["children"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .olsTerms(x = ans)
}

##' @export
##' @rdname olsTerms
parents <- function(object) {
    stopifnot(inherits(object, "olsTerm"))
    if (object@is_root)
        return(NULL)
    url <- termLinks(object)[["parents"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .olsTerms(x = ans)
}

##' @export
##' @rdname olsTerms
ancestors <- function(object) {
    stopifnot(inherits(object, "olsTerm"))
    if (object@is_root)
        return(NULL)
    url <- termLinks(object)[["ancestors"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .olsTerms(x = ans)
}

##' @export
##' @rdname olsTerms
descendants <- function(object) {
    stopifnot(inherits(object, "olsTerm"))
    if (!object@has_children)
        return(NULL)
    url <- termLinks(object)[["descendants"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .olsTerms(x = ans)
}

##########################################
## show methods

##' @export
##' @rdname olsTerms
setMethod("show", "olsTerm",
          function(object) {
              ids <- .termId(object)
              cat("A olsTerm from the", termPrefix(object), "ontology:", ids, "\n")
              cat(" Label: ", termLabel(object),"\n  ", sep = "")
              desc <- termDesc(object)
              if (is.null(desc)) cat("No description\n")
              else for (i in seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })

##' @export
##' @rdname olsTerms
setMethod("show", "olsTerms",
          function(object) {
              cat("Object of class 'olsTerms' with", length(object), "entries\n")
              onts <- unique(termPrefix(object))
              if (length(onts) == 1)
                  cat(" From the", onts, "ontology\n")
              else if (length(onts) < 6)
                  cat(" From the", paste(onts, collapse = ", "), "ontologies\n")
              else cat(" From ", length(onts), "ontologies\n")
              n <- length(object)
              if (n > 4)
                  cat(" ", paste(head(termId(object), n=2), collapse = ", "),
                      "...",
                      paste(tail(termId(object), n=2), collapse = ", "), "\n")
              else
                  cat(paste(termId(object)[1:n], collapse = ", "), "\n")
          })

##########################################
## Accessors
##' @export
##' @rdname olsTerms
setMethod("termSynonym", "olsTerm",
          function(object) unlist(object@synonyms))
##' @export
##' @rdname olsTerms
setMethod("termSynonym", "olsTerms",
          function(object) lapply(object@x, termSynonym))
##' @export
##' @rdname olsTerms
setMethod("isObsolete", "olsTerm",
          function(object) object@is_obsolete)
##' @export
##' @rdname olsTerms
setMethod("isObsolete", "olsTerms",
          function(object) sapply(object@x, isObsolete))
##' @export
##' @rdname olsTerms
setMethod("isRoot", "olsTerm",
          function(object) object@is_root)
##' @export
##' @rdname olsTerms
setMethod("isRoot", "olsTerms",
          function(object) sapply(object@x, isRoot))
##' @export
##' @rdname olsTerms
setMethod("termLabel", "olsTerm",
          function(object) object@label)
##' @export
##' @rdname olsTerms
setMethod("termLabel", "olsTerms",
          function(object) sapply(object@x, termLabel))
##' @export
##' @rdname olsTerms
setMethod("termId", "olsTerm",
          function(object) .termId(object))
##' @export
##' @rdname olsTerms
setMethod("termId", "olsTerms",
          function(object) sapply(object@x, .termId))
##' @export
##' @rdname olsTerms
setMethod("termLinks", "olsTerm",
          function(object) {
              links <- unlist(object@links)
              names(links) <- sub("\\.href", "", names(links))
              links
          })
##' @export
##' @rdname olsTerms
setMethod("termPrefix", "olsTerm",
          function(object) object@ontology_prefix)
##' @export
##' @rdname olsTerms
setMethod("termPrefix", "olsTerms",
          function(object) sapply(object@x, termPrefix))
##' @export
##' @rdname olsTerms
setMethod("termDesc", "olsTerm",
          function(object) unlist(object@description))
##' @export
##' @rdname olsTerms
setMethod("termDesc", "olsTerms",
          function(object) sapply(object@x, termDesc))
##' @export
##' @rdname olsTerms
setMethod("termOntology", "olsTerm",
          function(object) unlist(object@ontology_name))
##' @export
##' @rdname olsTerms
setMethod("termOntology", "olsTerms",
          function(object) sapply(object@x, termOntology))
##' @export
##' @rdname olsTerms
setMethod("termNamespace", "olsTerm",
          function(object) unlist(object@annotation$has_obo_namespace))
##' @export
##' @rdname olsTerms
setMethod("termNamespace", "olsTerms",
          function(object) sapply(object@x, termNamespace))

##########################################
## Data manipulation

##' @export
##' @rdname olsTerms
setMethod("length", "olsTerms", function(x) length(x@x))
##' @export
##' @rdname olsTerms
setMethod("unique", "olsTerms", function(x) x[!duplicated(names(x@x))])
##' @export
##' @rdname olsTerms
##'
##' @param x a `olsTerms` object.
##'
##' @param i index of elecements to subset.
##'
##' @param j ignored.
##'
##' @param drop ignored.
setMethod("[", "olsTerms",
          function(x, i, j="missing", drop="missing")
              .olsTerms(x = x@x[i]))
##' @export
##' @rdname olsTerms
setMethod("[[", "olsTerms",
          function(x, i, j="missing", drop="missing") x@x[[i]])
##' @export
##'
##' @param X `olsTerms` object.
##'
##' @param FUN a `function` to be applied to each `olsTerm` element of
##'     `X`.
##'
##' @param ... additional arguments passed to `FUN`.
##'
##' @rdname olsTerms
setMethod("lapply", "olsTerms",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
## ##' @export
## setMethod("all.equal", c("olsTerm", "olsTerm"),
##           function(target, current) {
##               msg <- Biobase::validMsg(NULL, NULL)
##               snms <- slotNames("olsTerm")
##               for (i in snms[-grep("links", snms)]) {
##                   eq <- all.equal(slot(target, i), slot(current, i))
##                   if (is.character(eq)) {
##                       eq <- paste0("Slot '", i, "': ", eq)
##                       msg <- Biobase:::validMsg(msg, eq)
##                   }
##               }
##               lt <- slot(target, "links")
##               lc <- slot(current, "links")
##               ot <- order(names(lt))
##               oc <- order(names(lc))
##               msg <- Biobase:::validMsg(msg, all.equal(lt[ot], lc[oc]))
##               if (is.null(msg)) return(TRUE)
##               else msg
##           })


## setMethod("all.equal", c("olsTerms", "olsTerms"),
##           function(target, current) {
##               msg <- Biobase::validMsg(NULL, NULL)
##               if (length(target) != length(current)) {
##                   msg <- Biobase::validMsg(msg, "2 olsTerms are of different lengths")
##               } else {
##                   tg <- target@x
##                   ct <- current@x
##                   if (any(sort(names(tg)) != sort(names(ct)))) {
##                       msg <- Biobase::validMsg(msg, "olsTerm ids don't match")
##                   } else {
##                       ot <- order(names(tg))
##                       oc <- order(names(ct))
##                       tg <- tg[ot]
##                       ct <- ct[oc]
##                       for (i in seq_along(tg)) {
##                           eq <- all.equal(tg[[i]], ct[[i]])
##                           if (is.character(eq)) {
##                               eq <- paste0("olsTerm id '", names(tg)[i], "': ", eq)
##                               msg <- Biobase:::validMsg(msg, eq)
##                           }
##                       }
##                   }
##               }
##               if (is.null(msg)) return(TRUE)
##               else msg
##           })

##' @export
setAs("olsTerm", "data.frame",
      function(from)
          data.frame(
              id = fix_null(termId(from)),
              label = fix_null(termLabel(from)),
              description = fix_null(termDesc(from)),
              ontology = fix_null(termOntology(from)),
              is_obsolete = fix_null(isObsolete(from)),
              has_children = fix_null(from@has_children),
              is_root = fix_null(isRoot(from)),
              first_synonym = fix_null(termSynonym(from)),
              iri = fix_null(from@iri),
              is_defining_ontology = fix_null(from@is_defining_ontology),
              stringsAsFactors = FALSE)
          )

##' @export
##' @rdname olsTerms
as.olsTerm.data.frame <- function(x)
    as(x, "data.frame")

##' @export
setAs("olsTerms", "data.frame",
      function(from) do.call(rbind, lapply(from, as, "data.frame")))

##' @export
##' @rdname olsTerms
as.olsTerms.data.frame <- function(x)
    as(x, "data.frame")

#############################################
## helper functions
makeOlsTerms <- function(oid, pagesize, obsolete) {
    ont <- olsOntology(oid)
    url <- paste0(olsLinks(ont)[["terms"]], "?")
    if (!is.null(obsolete))
        url <- paste0(url, "obsoletes=",
                      ifelse(obsolete, "true", "false"))
    url <- paste0(url, "&size=", as.integer(pagesize))
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .olsTerms(x = ans)
}

termFromJson <- function(x) {
    .olsTerm(iri = x[["iri"]],
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
}

fix_null <- function(x) {
    if (is.null(x)) return(NA)
    if (is.list(x)) return(x[[1]])
    return(x)
}

.termId <- function(x) x@obo_id
