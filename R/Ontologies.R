##' @title olsOntologies
##'
##' @aliases olsOntologies olsOntology
##' @aliases olsLinks olsLinks,olsOntology
##' @aliases olsConfig olsConfig,olsOntology
##' @aliases olsVersion olsVersion,character olsVersion,olsOntology olsVersion,olsOntologies
##' @aliases olsLoaded olsLoaded,character olsLoaded,olsOntology olsLoaded,olsOntologies
##' @aliases olsUpdated olsUpdated,character olsUpdated,olsOntology olsUpdated,olsOntologies
##' @aliases olsStatus olsStatus,character olsStatus,olsOntology olsStatus,olsOntologies
##' @aliases olsPrefix olsPrefix,character olsPrefix,olsOntology olsPrefix,olsOntologies
##' @aliases olsDesc olsDesc,character olsDesc,olsOntology olsDesc,olsOntologies
##' @aliases olsTitle olsTitle,character olsTitle,olsOntology olsTitle,olsOntologies
##' @aliases olsNamespace olsNamespace,character olsNamespace,olsOntology olsNamespace,olsOntologies
##' @aliases ontologyUrl ontologyUrl,character ontologyUrl,olsOntology
##' @aliases as.data.frame.olsOntologies
##'
##' @description
##'
##' The rols package provides an interface to PRIDE's Ontology Lookup
##' Servive (OLS) and can be used to query one or multiple ontologies,
##' stored as `olsOntology` and `olsOntologies` instances, and
##' containing various information as provided by OLS.
##'
##' @details
##'
##' Ontologies are referred to by their namespace, which is lower
##' case: the Gene Onology is "go", the Mass spectrometry ontology is
##' "ms", etc. The ontologies also have prefixes, which are upper
##' case: the Gene Onology prefix "GO", the Mass spectrometry ontology
##' prefix "MS". One exception to this rule is the Drosophila
##' Phenotype Ontology, whose namespace and prefix are "dpo" and
##' "FBcv" respectively (there might be more). This is particularly
##' confusing as the FlyBase Controlled Vocabulary has "fbcv" and
##' "FBcv" as namespace and prefix respectively.
##'
##' When using a character to initialise an ontology or query a term,
##' "fbcv" (this is case insensitive) will refer to the the FlyBase
##' Controlled Vocabulary. The the Drosophila Phenotype Ontology will
##' have to be referred as "dpo" (also case insensitive).
##'
##' @section Constructors:
##'
##' Objects can be created in multiple ways. The [olsOntologies()]
##' function will initialise all available ontolgies as an
##' `olsOntologies` object, while a call to [olsOntology()] with an
##' ontology namespace or prefix as argument will initialise the
##' ontology of interest as an `olsOntology` instance.
##'
##' `Ontolgies` instances can be subset with `[` and `[[` (using their
##' namespace, see Details) and iterated over with
##' `lapply`. `Ontolgies` can be converted into a simple `data.frame`
##' containing the ontology prefixes, namespaces and titles using
##' `as(., "data.frame")`. `olsOntologies` can also be coerced to
##' lists of `olsOntology` ojects with `as(., "list")`.
##'
##' @section Accessors:
##'
##' - `olsDesc(object = "olsOntology")` returns the description of an
##'   ontology. Also works for `olsOntologies` objects and a `character`
##'   describing an ontology namespace or prefix (see Details).
##'
##' - `olsPrefix(object = "olsOntology")` retruns the prefix of an
##'   ontology. Also works for `olsOntologies` objects and a `character`
##'   describing an ontology namespace or prefix (see Details).
##'
##' - `olsVersion(object = "olsOntology")` returns the version of the
##'   ontology. Also works with an a `character` defining an ontology
##'   namespace or prefix (see Details) or an object of class
##'   `olsOntologies`, in which case it returns a list of versions.
##'
##' - `olsLoaded(object = "olsOntology")` returns the loading date of the
##'   ontology. Also works with a `character` containing the ontology
##'   namespace or prefix (see Details) or an object of class
##'   `olsOntologies`.
##'
##' - `olsUpdated(object = "olsOntology")` returns the update date of the
##'   ontology. Also works with a `character` containing the ontology
##'   namespace or prefix (see Details) or an object of class
##'   `olsOntologies`.
##'
##' - `olsStatus(object = "olsOntology")` returns the status of the
##'    ontology. Also works with a `character` containing the ontology
##'    namespace or prefix (see Details) or an object of class
##'    `olsOntologies`.
##'
##' - `olsTitle(object = "olsOntology")` returns the title of an
##'    ontology. Also works with a `character` containing the ontology
##'    namespace or prefix (see Details) or an object of class
##'    `olsOntologies`.
##'
##' - `olsNamespace(object = "olsOntology")` returns the namespace of an
##'   ontology. Also works with a `character` containing the ontology
##'   namespace or prefix (see Details) or an object of class
##'   `olsOntologies`.
##'
##' - `olsLinks(object = "olsOntology")` returns a named `character` with
##'   hyperlink to the ontology itself, and other associated concepts
##'   such as its terms.
##'
##' - `olsConfig(object = "olsOntology")` returns a list of additional
##'    unstructured, partly redundant information about the ontology.
##'
##' - `ontologyUrl(object = "olsOntology") return the hyperlink to the
##'   ontology itself. It can also be used with a `character` defining
##'   the namespace or prefix of an ontology, in which case it is
##'   created from the base OLS API URL.
##'
##' @section Ontology terms:
##'
##' Once an ontology has been created an an `olsOntology` instance, all
##' its terms can be requested using the `Terms()` constructor. See
##' [Terms()] for details.
##'
##' @references
##'
##' - OLS3 API (the OLS4 API should function identically to the OLS3):
##'   <http://www.ebi.ac.uk/ols4/ols3help>
##'
##' - REST API for OLS: <https://www.ebi.ac.uk/ols4/swagger-ui/index.html>
##'
##' @author Laurent Gatto
##'
##' @name olsOntologies
##'
##' @examples
##'
##' #############################
##' ## All ontologies
##' (onts <- olsOntologies())
##'
##' #############################
##' ## Alzheimer's Disease Ontology (ADO)
##' ## 1. From the ontologies object
##' (ado1 <- onts[['ado']])
##' ## 2. Create from its namespace
##' (ado2 <- olsOntology('ado')) ## also works with ADO
##'
##' all.equal(ado1, ado2)
##'
##' olsVersion(ado1)
##' olsPrefix(ado1)
##' olsNamespace(ado1)
##' olsTitle(ado1)
##' olsDesc(ado1)
##' olsLinks(ado1)
##' str(olsConfig(ado1))
NULL

############################################################
## A single ontology
.olsOntology <- setClass("olsOntology",
                         slots = c(
                             languages = "list",
                             lang = "character",
                             ontologyId = "character",
                             loaded = "NullOrChar",
                             updated = "NullOrChar",
                             status = "NullOrChar",
                             message = "NullOrChar",
                             version = "NullOrChar",
                             numberOfTerms = "integer",
                             numberOfProperties = "integer",
                             numberOfIndividuals = "integer",
                             config = "list",
                             links = "list"
                         ))

############################################################
## A list of Ontology instances
.olsOntologies <- setClass("olsOntologies", slots = c(x = "list"))

##########################################
## Constructors

##' @export
##'
##' @param object an instance of class `olsOntologies` or `olsOntology`. For
##'     some functions, a ontology identifier is applicable.
##'
##' @rdname olsOntologies
setMethod("olsOntologies", "missing",
          function(object) makeOlsOntologies())

##' @export
##' @rdname olsOntologies
setMethod("olsOntology", "character",
          function(object) {
              ## make urls from ontologyId
              url <- ontologyUrl(object)
              makeOlsOntology(url)
          })

##' @export
##' @rdname olsOntologies
setMethod("olsOntology", "olsOntology",
          function(object) object)

##########################################
## show methods
##' @export
##' @rdname olsOntologies
setMethod("show", "olsOntology",
          function(object) {
              cat("olsOntology: ", olsTitle(object),
                  " (", olsNamespace(object) , ")", sep = "")
              cat("  ", strwrap(olsDesc(object)), sep = "\n  ")
              cat("   Loaded:", olsLoaded(object),
                  "Updated:", olsUpdated(object),
                  "Version:", olsVersion(object), "\n")
              cat("  ", object@numberOfTerms, "terms ",
                  object@numberOfProperties, "properties ",
                  object@numberOfIndividuals, "individuals\n")
          })

##' @export
##' @importFrom utils head tail
##' @rdname olsOntologies
setMethod("show", "olsOntologies",
          function(object) {
              cat("Object of class 'olsOntologies' with",
                  length(object), "entries\n")
              if (length(object) > 4)
                  cat("  ", paste(head(olsPrefix(object), n=2),
                                  collapse = ", "),
                      "...",
                      paste(tail(olsPrefix(object), n=2),
                            collapse = ", "), "\n")
              else
                  cat(paste(olsPrefix(object)[1:length(object)],
                            collapse = ", "), "\n")
          })

##########################################
## Accessors

##' @export
##' @rdname olsOntologies
setMethod("olsVersion", "character",
          function(object) olsVersion(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsVersion", "olsOntology",
          function(object) object@config$version)
##' @export
##' @rdname olsOntologies
setMethod("olsVersion", "olsOntologies",
          function(object) sapply(object@x, olsVersion))

##' @export
##' @rdname olsOntologies
setMethod("olsLoaded", "character",
          function(object) olsLoaded(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsLoaded", "olsOntology",
          function(object) substr(object@loaded, 1, 10))
##' @export
##' @rdname olsOntologies
setMethod("olsLoaded", "olsOntologies",
          function(object) sapply(object@x, olsLoaded))
##' @export
##' @rdname olsOntologies
setMethod("olsLinks", "olsOntology",
          function(object) {
              links <- unlist(object@links)
              names(links) <- sub("\\.href", "", names(links))
              links
          })
##' @export
##' @rdname olsOntologies
setMethod("olsConfig", "olsOntology",
          function(object) object@config)
##' @export
##' @rdname olsOntologies
setMethod("olsUpdated", "character",
          function(object) olsUpdated(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsUpdated", "olsOntology",
          function(object) substr(object@updated, 1, 10))
##' @export
##' @rdname olsOntologies
setMethod("olsUpdated", "olsOntologies",
          function(object) sapply(object@x, olsUpdated))
##' @export
##' @rdname olsOntologies
setMethod("olsPrefix", "character",
          function(object) olsPrefix(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsPrefix", "olsOntology",
          function(object) object@config$preferredPrefix)
##' @export
##' @rdname olsOntologies
setMethod("olsPrefix", "olsOntologies",
          function(object) sapply(object@x, olsPrefix))
##' @export
##' @rdname olsOntologies
setMethod("olsDesc", "character",
          function(object) olsDesc(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsDesc", "olsOntology",
          function(object) object@config$description)
##' @export
##' @rdname olsOntologies
setMethod("olsDesc", "olsOntologies",
          function(object) sapply(object@x, olsDesc))
##' @export
##' @rdname olsOntologies
setMethod("olsTitle", "character",
          function(object) olsTitle(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsTitle", "olsOntology",
          function(object) object@config$title)
##' @export
##' @rdname olsOntologies
setMethod("olsTitle", "olsOntologies",
          function(object) sapply(object@x, olsTitle))
##' @export
##' @rdname olsOntologies
setMethod("olsStatus", "character",
          function(object) olsStatus(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsStatus", "olsOntology",
          function(object) object@status)
##' @export
##' @rdname olsOntologies
setMethod("olsStatus", "olsOntologies",
          function(object) sapply(object@x, olsStatus))
##' @export
##' @rdname olsOntologies
setMethod("olsNamespace", "character",
          function(object) olsNamespace(olsOntology(object)))
##' @export
##' @rdname olsOntologies
setMethod("olsNamespace", "olsOntology",
          function(object) object@config$namespace)
##' @export
##' @rdname olsOntologies
setMethod("olsNamespace", "olsOntologies",
          function(object) sapply(object@x, olsNamespace))
##' @export
##' @rdname olsOntologies
setMethod("ontologyUrl", "character",
          function(object)
              paste0("https://www.ebi.ac.uk/ols4/api/ontologies/",
                     object))
##' @export
##' @rdname olsOntologies
setMethod("ontologyUrl", "olsOntology",
          function(object) olsLinks(object)[["self"]])

##########################################
## Data manipulation
##' @export
##'
##' @param X `olsOntologies` object.
##'
##' @param FUN a `function` to be applied to each `olsOntology` element
##'     of `X`.
##'
##' @param ... additional arguments passed to `FUN`.
##'
##' @rdname olsOntologies
setMethod("lapply", "olsOntologies",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
##' @export
##'
##' @param x an `olsOntologies` object.
##'
##' @param i index of elecements to subset.
##'
##' @param j ignored.
##'
##' @param drop ignored.
##'
##' @rdname olsOntologies
setMethod("[", "olsOntologies",
          function(x, i, j="missing", drop="missing")
              new("olsOntologies", x = x@x[i]))
##' @export
##' @rdname olsOntologies
setMethod("[[", "olsOntologies",
          function(x, i, j="missing", drop="missing") {
              if (is.numeric(i)) {
                  i <- as.integer(i)
                  return(x@x[[i]])
              }
              if (is.character(i)) {
                  nms <- olsNamespace(x)
                  k <- which(nms %in% i)
                  if (!length(k))
                      stop("Ontology not found.")
                  if (length(k) > 1)
                      stop("Ontology not unique.")
                  return(x[[k]])
              }
              stop("'i' must be a character or a numeric.")
          })
##' @export
##' @rdname olsOntologies
setMethod("length", "olsOntologies", function(x) length(x@x))

## This will not always be the correct URI (see for example
## Orphaned/ORBO and https://github.com/EBISPOT/OLS/issues/35)
## setMethod("ontologyUri", "missing",
##           function(encode = TRUE) {
##               uri <- "http://purl.obolibrary.org/obo/"
##               if (encode)
##                   uri <- gsub("%", "%25", URLencode(uri, TRUE))
##               uri
##           })

## setMethod("ontologyUri", "Ontology",
##           function(object, encode = TRUE, withPrefix = FALSE) {
##               uri <- object@config$baseUris
##               if (is.null(uri) | length(uri) == 0)
##                   return(ontologyUri())
##               if (length(uri) > 1) {
##                   msg <- paste0("More than one URI available:\n  ",
##                                 paste(unlist(uri), collapse = ", "), "\n  ",
##                                 "Choosing the first one.\n")
##                   warning(msg)
##               }
##               uri <- uri[[1]][1]
##               if (!withPrefix)
##                   uri <- sub("/[A-Za-z]+_$", "/", uri)
##               if (encode)
##                   uri <- gsub("%", "%25", URLencode(uri, TRUE))
##               uri
##           })

##########################################
## Coercion

##' @import methods
##' @export
setAs("olsOntologies", "data.frame",
      function(from) as.data.frame.olsOntologies(from))

##' @export
as.data.frame.olsOntologies <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
    .as_vector <- function(x) {
        if (is.list(x))
            x <- sapply(x, paste, collapse = "; ")
        x
    }
    pre <- .as_vector(olsPrefix(x))
    nms <- .as_vector(olsNamespace(x))
    ttl <- .as_vector(olsTitle(x))
    data.frame(Prefix = pre,
               Namespace = nms,
               Title = ttl)
}

##' @export
setAs("olsOntologies", "list", function(from) from@x)

## ## Ontologies aren't names anymore (for now)
## setMethod("all.equal", c("Ontologies", "Ontologies"),
##           function(target, current) {
##               msg <- Biobase::validMsg(NULL, NULL)
##               if (length(target) != length(current)) {
##                   msg <- Biobase::validMsg(msg, "The 2 Ontologies are of different lengths")
##               } else {
##                   tg <- target@x
##                   ct <- current@x
##                   if (any(sort(names(tg)) != sort(names(ct)))) {
##                       msg <- validMsg(msg, "Ontology names don't match")
##                   } else {
##                       ## reorder before comparing Ontolgy objects one
##                       ## by one
##                       tg <- tg[order(names(tg))]
##                       ct <- ct[order(names(ct))]
##                       for (i in seq_along(tg)) {
##                           eq <- all.equal(tg[[i]], ct[[i]])
##                           if (is.character(eq)) {
##                               eq <- paste0("Ontology '", names(tg)[i], "': ", eq)
##                               msg <- validMsg(msg, eq)
##                           }
##                       }
##                   }
##               }
##               if (is.null(msg)) return(TRUE)
##               else msg
##           })

##' @importFrom Biobase validMsg
## ##' @export
## setMethod("all.equal", c("Ontology", "Ontology"),
##           function(target, current) {
##               msg <- Biobase::validMsg(NULL, NULL)
##               sn <- slotNames("Ontology")
##               sn0 <- sn[sn != "config"]
##               for (i in sn0) {
##                   eq <- all.equal(slot(current, i), slot(target, i))
##                   if (is.character(eq))
##                   msg <- validMsg(msg, paste0(i, ": ", eq))
##               }
##               c1 <- slot(current, "config")
##               c2 <- slot(target, "config")
##               c1 <- c1[order(names(c1))]
##               c2 <- c2[order(names(c2))]
##               msg <- Biobase::validMsg(msg, all.equal(c1, c2))
##               if (is.null(msg)) TRUE else msg
##           })


##########################################
## Helper functions
makeOlsOntologies <- function() {
    url <- "https://www.ebi.ac.uk/ols4/api/ontologies"
    .olsOntologies(x = lapply(ols_requests(url, "ontologies"),
                              ontologyFromJson))
}

makeOlsOntology <- function(url) {
    request(url) |>
        req_perform() |>
        resp_body_json() |>
        ontologyFromJson()
}

ontologyFromJson <- function(x) {
    .olsOntology(languages = x[["languages"]],
              lang = x[["lang"]],
              ontologyId = x[["ontologyId"]],
              loaded = x[["loaded"]],
              updated = x[["updated"]],
              status = x[["status"]],
              message = x[["message"]],
              version = x[["version"]],
              numberOfTerms = x[["numberOfTerms"]],
              numberOfProperties = x[["numberOfProperties"]],
              numberOfIndividuals = x[["numberOfIndividuals"]],
              config = x[["config"]],
              links = x[["_links"]])
}
