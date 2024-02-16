##' @title Ontology Terms
##'
##' @aliases Term Terms Terms,character Terms,Ontology
##' @aliases termLinks termLinks,Term
##' @aliases children parents ancestors descendants
##' @aliases termSynonym termSynonym,Term termSynonym,Terms
##' @aliases isObsolete isObsolete,Term isObsolete,Terms
##' @aliases isRoot isRoot,Term isRoot,Terms
##' @aliases termLabel termLabel,Term termLabel,Terms
##' @aliases termId termId,Term termId,Terms
##' @aliases termPrefix termPrefix,Term termPrefix,Terms
##' @aliases termDesc termDesc,Term termDesc,Terms
##' @aliases termOntology termOntology,Term termOntology,Terms
##' @aliases termNamespace termNamespace,Term termNamespace,Terms
##'
##' @description
##'
##' The `Term` class describes an ontology term. A set of terms are
##' instantiated as a `Terms` class.
##'
##' @section Contructors:
##'
##' Objects can be created using the `Term()` and `Terms()`
##' constructers. The latter is used with an object of class
##' `Ontology` or a `character` describing a valid ontology prefix to
##' download and instantiate all terms of an ontology of interest. The
##' former takes an `Ontology` object (or an ontology prefix) and
##' a term identifer to instantiate that specific term.
##'
##' For any given `Term` object, the `children`, `parents`,
##' `ancestors` and `descendants` terms can be generated with the
##' `children()`, `parents()`, `ancestor()` and `descendants()`
##' function. `Terms` instances can be subset with `[` and `[[` and
##' iterated over with `lapply`.
##'
##' @section Accessors:
##'
##' - `isObsolete(object = "Term")` returns a `TRUE` if the term is
##'   obsolete, `FALSE` otherwise. Also works on `Terms` instances.
##'
##' - `isRoot(object = "Term")` returns a `TRUE` if the term is a root
##'   term, `FALSE` otherwise. Also works on `Terms` instances.
##'
##' - `termDesc(object = "Term")` returns a `character` with the
##'   term's description. Also works on `Terms` instances.
##'
##' - `termId(object = "Term")` returns a `character` with the term's
##'   identifier. Also works on `Terms` instances.
##'
##' - `termLabel(object = "Term")` returns a `character` with the
##'   term's label. Also works on `Terms` instances.
##'
##' - `termNamespace(object = "Term")` returns a `character` with the
##'   term's namespace. Also works on `Terms` instances.
##'
##' - `termOntology(object = "Term")` returns a `character` with the
##'   term's ontology (where it was retrieved from). Also works on
##'   `Terms` instances.
##'
##' - `termPrefix(object = "Term")` returns a `character` with the
##'   term's (ontology) prefix (where it was retrieved from). Also
##'   works on `Terms` instances.
##'
##' - `termSynonym(object = "Term")` returns a `character` with the
##'   term's synpnym(s). Also works on `Terms` instances.
##'
##' - `termLinks(object = "Term")` returns a named `character` with
##'   hyperlink to/from the term.
##'
##' @section Related terms:
##'
##' - `children(object = "Term")` returns a new `Terms` instance with
##'    the `object`'s children or `NULL` if there are no children.
##'
##' - `parents(object = "Term")` returns a new `Terms` instance with
##'   the `object`'s parents or `NULL` if there are no parents.
##'
##' - `ancestors(object = "Term")` returns a new `Terms` instance with
##'   the `object`'s ancestors or `NULL` if there are no ancestors.
##'
##' - `descendants(object = "Term")` returns a new `Terms` instance
##'   with the `object`'s descendants or `NULL` if there are no
##'   descendants.
##'
##' @section Coercion:
##'
##' - `as(x, "data.fram")` coerces a `Term` or `Terms` instance into a
##'   `data.frame` of length 1 (for the former) or length `length(x)`
##'   for the latter. The result will contain the following columns:
##'   id, label, description of the term(s), their ontology, whether
##'   they are obsolete, have children or are root node, the first
##'   synonym only, their iri and whether they are defining the
##'   ontology. Any missing value will be reported as `NA`.
##'
##' @name Terms
##'
##' @author Laurent Gatto
##'
##' @examples
##'
##' ## Alzheimer's Disease Ontology (ADO)
##' (adoterms <- Terms('ado'))
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
##' Term("ado", "ADO:0000090")
NULL

############################################################
## A single term
.Term <- setClass("Term",
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
.Terms <- setClass("Terms", slots = c(x = "list"))

##########################################
## Constructors

##' @export
##' @rdname Terms
##'
##' @param object generally an instance of class `Terms` or `Term`. In
##'     some cases, an ontology identifier is applicable.
##'
##' @param pagesize `numeric(1)`, converted to an integer, defining
##'     the response page size. Default is 1000.
##'
##' @param obsolete `NULL` or `logical(1)` defining whether obsolete
##'     terms (`TRUE`), current terms (`FALSE`) or all (`NULL`,
##'     default) should be returned.
setMethod("Terms", "character", ## ontologyId
          function(object, pagesize = 1000, obsolete = NULL)
              makeTerms(object, pagesize, obsolete))

##' @export
##' @rdname Terms
setMethod("Terms", "Ontology",
          function(object, pagesize = 1000, obsolete = NULL)
              makeTerms(object, pagesize, obsolete))

##' @export
##' @rdname Terms
##'
##' @param id `character(1)` with the term's identifier.
setMethod("Term", "character",
          function(object, id) Term(Ontology(object), id))

##' @export
##' @rdname Terms
setMethod("Term", "Ontology",
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
##' @rdname Terms
children <- function(object) {
    stopifnot(inherits(object, "Term"))
    if (!object@has_children)
        return(NULL)
    url <- termLinks(object)[["children"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .Terms(x = ans)
}

##' @export
##' @rdname Terms
parents <- function(object) {
    stopifnot(inherits(object, "Term"))
    if (object@is_root)
        return(NULL)
    url <- termLinks(object)[["parents"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .Terms(x = ans)
}

##' @export
##' @rdname Terms
ancestors <- function(object) {
    stopifnot(inherits(object, "Term"))
    if (object@is_root)
        return(NULL)
    url <- termLinks(object)[["ancestors"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .Terms(x = ans)
}

##' @export
##' @rdname Terms
descendants <- function(object) {
    stopifnot(inherits(object, "Term"))
    if (!object@has_children)
        return(NULL)
    url <- termLinks(object)[["descendants"]]
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .Terms(x = ans)
}

##########################################
## show methods

##' @export
##' @rdname Terms
setMethod("show", "Term",
          function(object) {
              ids <- .termId(object)
              cat("A Term from the", termPrefix(object), "ontology:", ids, "\n")
              cat(" Label: ", termLabel(object),"\n  ", sep = "")
              desc <- termDesc(object)
              if (is.null(desc)) cat("No description\n")
              else for (i in seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })

##' @export
##' @rdname Terms
setMethod("show", "Terms",
          function(object) {
              cat("Object of class 'Terms' with", length(object), "entries\n")
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
##' @rdname Terms
setMethod("termSynonym", "Term",
          function(object) unlist(object@synonyms))
##' @export
##' @rdname Terms
setMethod("termSynonym", "Terms",
          function(object) lapply(object@x, termSynonym))
##' @export
##' @rdname Terms
setMethod("isObsolete", "Term",
          function(object) object@is_obsolete)
##' @export
##' @rdname Terms
setMethod("isObsolete", "Terms",
          function(object) sapply(object@x, isObsolete))
##' @export
##' @rdname Terms
setMethod("isRoot", "Term",
          function(object) object@is_root)
##' @export
##' @rdname Terms
setMethod("isRoot", "Terms",
          function(object) sapply(object@x, isRoot))
##' @export
##' @rdname Terms
setMethod("termLabel", "Term",
          function(object) object@label)
##' @export
##' @rdname Terms
setMethod("termLabel", "Terms",
          function(object) sapply(object@x, termLabel))
##' @export
##' @rdname Terms
setMethod("termId", "Term",
          function(object) .termId(object))
##' @export
##' @rdname Terms
setMethod("termId", "Terms",
          function(object) sapply(object@x, .termId))
##' @export
##' @rdname Terms
setMethod("termLinks", "Term",
          function(object) {
              links <- unlist(object@links)
              names(links) <- sub("\\.href", "", names(links))
              links
          })
##' @export
##' @rdname Terms
setMethod("termPrefix", "Term",
          function(object) object@ontology_prefix)
##' @export
##' @rdname Terms
setMethod("termPrefix", "Terms",
          function(object) sapply(object@x, termPrefix))
##' @export
##' @rdname Terms
setMethod("termDesc", "Term",
          function(object) unlist(object@description))
##' @export
##' @rdname Terms
setMethod("termDesc", "Terms",
          function(object) sapply(object@x, termDesc))
##' @export
##' @rdname Terms
setMethod("termOntology", "Term",
          function(object) unlist(object@ontology_name))
##' @export
##' @rdname Terms
setMethod("termOntology", "Terms",
          function(object) sapply(object@x, termOntology))
##' @export
##' @rdname Terms
setMethod("termNamespace", "Term",
          function(object) unlist(object@annotation$has_obo_namespace))
##' @export
##' @rdname Terms
setMethod("termNamespace", "Terms",
          function(object) sapply(object@x, termNamespace))

##########################################
## Data manipulation

##' @export
##' @rdname Terms
setMethod("length", "Terms", function(x) length(x@x))
##' @export
##' @rdname Terms
setMethod("unique", "Terms", function(x) x[!duplicated(names(x@x))])
##' @export
##' @rdname Terms
##'
##' @param x a `Terms` object.
##'
##' @param i index of elecements to subset.
##'
##' @param j ignored.
##'
##' @param drop ignored.
setMethod("[", "Terms",
          function(x, i, j="missing", drop="missing")
              .Terms(x = x@x[i]))
##' @export
##' @rdname Terms
setMethod("[[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[[i]])
##' @export
##'
##' @param X `Terms` object.
##'
##' @param FUN a `function` to be applied to each `Term` element of
##'     `X`.
##'
##' @param ... additional arguments passed to `FUN`.
##'
##' @rdname Terms
setMethod("lapply", "Terms",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
## ##' @export
## setMethod("all.equal", c("Term", "Term"),
##           function(target, current) {
##               msg <- Biobase::validMsg(NULL, NULL)
##               snms <- slotNames("Term")
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


## setMethod("all.equal", c("Terms", "Terms"),
##           function(target, current) {
##               msg <- Biobase::validMsg(NULL, NULL)
##               if (length(target) != length(current)) {
##                   msg <- Biobase::validMsg(msg, "2 Terms are of different lengths")
##               } else {
##                   tg <- target@x
##                   ct <- current@x
##                   if (any(sort(names(tg)) != sort(names(ct)))) {
##                       msg <- Biobase::validMsg(msg, "Term ids don't match")
##                   } else {
##                       ot <- order(names(tg))
##                       oc <- order(names(ct))
##                       tg <- tg[ot]
##                       ct <- ct[oc]
##                       for (i in seq_along(tg)) {
##                           eq <- all.equal(tg[[i]], ct[[i]])
##                           if (is.character(eq)) {
##                               eq <- paste0("Term id '", names(tg)[i], "': ", eq)
##                               msg <- Biobase:::validMsg(msg, eq)
##                           }
##                       }
##                   }
##               }
##               if (is.null(msg)) return(TRUE)
##               else msg
##           })

##' @export
setAs("Term", "data.frame",
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
##' @rdname Terms
as.Term.data.frame <- function(x)
    as(x, "data.frame")

##' @export
setAs("Terms", "data.frame",
      function(from) do.call(rbind, lapply(from, as, "data.frame")))

##' @export
##' @rdname Terms
as.Terms.data.frame <- function(x)
    as(x, "data.frame")

#############################################
## helper functions
makeTerms <- function(oid, pagesize, obsolete) {
    ont <- Ontology(oid)
    url <- paste0(olsLinks(ont)[["terms"]], "?")
    if (!is.null(obsolete))
        url <- paste0(url, "obsoletes=",
                      ifelse(obsolete, "true", "false"))
    url <- paste0(url, "&size=", as.integer(pagesize))
    ans <- lapply(ols_requests(url, "terms"),
                  termFromJson)
    names(ans) <- sapply(ans, termId)
    .Terms(x = ans)
}

termFromJson <- function(x) {
    .Term(iri = x[["iri"]],
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
