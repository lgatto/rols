##' @title Querying OLS
##'
##' @aliases OlsSearch olsSearch
##' @aliases olsRows 'olsRows<-' allRows as.data.frame.OlsSearch
##'
##' @description
##'
##' Searching the Ontology Lookup Service is done first by creating an
##' object `OlsSearch` using the `OlsSearch()` constructor. Query
##' responses are then retrieved with the `olsSearch()` function.
##'
##' @author Laurent Gatto
##'
##' @name OlsSearch
##'
##' @examples
##'
##' ## Many results across all ontologies
##' OlsSearch(q = "trans-golgi")
##'
##' ## Exact matches
##' OlsSearch(q = "trans-golgi", exact = TRUE)
##'
##' ## Exact match in the gene ontology (go or GO) only
##' OlsSearch(q = "trans-golgi", exact = TRUE, ontology = "go")
##' OlsSearch(q = "trans-golgi", exact = TRUE, ontology = "GO")
##'
##' ## Exact match in the GO and Uberon
##' OlsSearch(q = "trans-golgi", exact = TRUE,
##'           ontology = c("GO", "Uberon"))
##'
##' ## Testing different ESI queries
##' OlsSearch(q = "electrospray", ontology = "MS")
##' OlsSearch(q = "ionization", ontology = "MS")
##' OlsSearch(q = "electrospray ionization", ontology = "MS")
##' OlsSearch(q = "electrospray ionization", ontology = "MS", exact=TRUE)
##'
##' ## Request 5 results instead of 20 (default)
##' OlsSearch(q = "plasma,membrane", ontology = "go", rows = 5)
##' ## Same as above
##' OlsSearch(q = "plasma membrane", ontology = "go", rows = 5)
##'
##' ## or, once the object was created
##' (res <- OlsSearch(q = "plasma,membrane", ontology = "go"))
##' olsRows(res) <- 5
##' res
##'
##' ## all results
##' res <- allRows(res)
##' res
##'
##' res <- OlsSearch(q = "trans-golgi", ontology = "go", rows = 5)
##' res
##' res <- olsSearch(res)
##' res
##' as(res, "data.frame")
##' trms <- as(res, "Terms")
##' trms
##' termPrefix(trms)
##' termId(trms)
##'
##' ## Setting rows and start parameters
##' tg1 <- OlsSearch(q = "trans-golgi", rows = 5, start = 0) |>
##'                  olsSearch() |>
##'                  as("data.frame")
##' tg2 <- OlsSearch(q = "trans-golgi", rows = 5, start = 5) |>
##'                  olsSearch() |>
##'                  as("data.frame")
##' tg3 <- OlsSearch(q = "trans-golgi", rows = 10, start = 0) |>
##'                  olsSearch() |>
##'                  as("data.frame")
##'
##' ## The two consecutive small results are identical
##' ## to the larger on.
##' identical(rbind(tg1, tg2), tg3)
############################################
## OlsSearch class
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
                                 local = "logical",
                                 childrenOf = "character",
                                 rows = "integer",
                                 start = "integer",
                                 url = "character",
                                 numFound = "integer",
                                 response = "data.frame"))

##########################################
## Constructor

##' @export
##'
##' @rdname OlsSearch
##'
##' @param q `characher(1)` containing the search query.
##'
##' @param ontology `character()` defining the ontology to be
##'     queried. Default is the empty character, to search all
##'     ontologies.
##'
##' @param type `character()` restricting the search to an entity
##'     type, one of `"class"`, `"property"`, `"individual"` or
##'     `"ontology"`.
##'
##' @param slim `character()` restricts the search to an particular
##'     set of slims by name.
##'
##' @param fieldList `character()` specifcies the fields to return.
##'     The defaults are iri, label, short_form, obo_id,
##'     ontology_name, ontology_prefix, description and type. Default
##'     is `""` for all fields.
##'
##' @param queryFields `character()` specifcies the fields to query,
##'     the defaults are label, synonym, description, short_form,
##'     obo_id, annotations, logical_description, iri. Default is `""`
##'     for all fields.
##'
##' @param exact `logical(1)` defining if exact matches should be
##'     returned. Default is `FALSE`.
##'
##' @param groupField `logical(1)`, set to `TRUE`rue to group results
##'     by unique id (IRI).
##'
##' @param obsoletes `logical(1)` defining whether obsolete terms
##'     should be queried. Default is `FALSE`.
##'
##' @param local `character(1)`, default is `FALSE`. Set to `TRUE` to
##'     only return terms that are in a defining ontology e.g. only
##'     return matches to gene ontology terms in the gene ontology,
##'     and exclude ontologies where those terms are also referenced
##'
##' @param childrenOf `character()` to restrict a search to children
##'     of a given term. Supply a list of IRI for the terms that you
##'     want to search under.
##'
##' @param rows `integer(1)` defining the number of query
##'     returns. Default is 20L. Maximum number of values returned by
##'     the server is 1000. To retrieve the next results, set `start`
##'     1000. See examle below.
##'
##' @param start `integer(1)` defining the results page.
##'     number. Default is 0L.
##'
##' @importFrom utils URLencode
OlsSearch <- function(q,
                      ontology = "",
                      type = "",
                      slim = "",
                      fieldList = "",
                      queryFields = "",
                      exact = FALSE,
                      groupField = FALSE,
                      obsoletes = FALSE,
                      local = TRUE,
                      childrenOf = "",
                      rows = 20L,
                      start = 0L) {
    if (missing(q))
        stop("You must supply a query.")
    if (rows > 1000) {
        warning("Setting row to max value 1000.")
        rows <- 1000
    }
    .args <- as.list(match.call())[-1]
    if (missing(rows))
        .args[["rows"]] <- rows
    ## Create search URL and instantiate OlsSearch object
    params <- c()
    for (i in seq_along(.args)) {
        nm <- names(.args)[i]
        arg <- .args[[i]]
        arg <- eval(arg, parent.frame())
        if (is.logical(arg))
            arg <- ifelse(arg, "true", "false")
        if (is.character(arg))
            arg <- URLencode(arg)
        if (nm == "ontology")
            arg <- tolower(arg)
        if (length(arg) > 1)
            arg <- paste(arg, collapse = ",")
        params <- append(params, paste(nm, arg, sep = "="))
    }
    ## searchUrl <- "http://www.ebi.ac.uk/ols/beta/api/search?"
    searchUrl <- "http://www.ebi.ac.uk/ols4/api/search?"
    url <- paste0(searchUrl,
                  paste(params, collapse = "&"))
    x <- request(url) |>
        req_perform() |>
        resp_body_json()
    numFound <- x[["response"]][["numFound"]]
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

##' @export
##'
##' @importFrom jsonlite fromJSON
##'
##' @rdname OlsSearch
##'
##' @param object `OlsSeach` result object.
##'
##' @param all `logical(1)` Should all rows be retrieved. Default is
##'     `FALSE`. Can also be set in the queary object directly with
##'     `allRows()`.
olsSearch <- function(object, all = FALSE) {
    if (all)
        object <- allRows(object)
    ans <- request(object@url) |>
        req_perform() |>
        resp_body_string() |>
        jsonlite::fromJSON()
    if (!length(ans[['response']][['docs']])) {
        object@response <- emptyQueryDataFrame
    } else {
        object@response <- ans[["response"]][["docs"]]
    }
    object
}


##########################################
## show method
##' @export
##' @rdname OlsSearch
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


##########################################
## Accessors and setter

##' @export
##'
##' @rdname OlsSearch
olsRows <- function(object) {
    stopifnot(inherits(object, "OlsSearch"))
    object@rows
}

##' @export
##'
##' @param value replacement value
##'
##' @rdname OlsSearch
"olsRows<-" <- function(object, value) {
    stopifnot(inherits(object, "OlsSearch"))
    stopifnot(is.numeric(value))
    object@url <- sub("rows=[0-9]+",
                      paste0("rows=", as.integer(value)),
                      object@url)
    object@rows <- as.integer(value)
    object
}

##' @export
##'
##' @rdname OlsSearch
allRows <- function(object) {
    stopifnot(inherits(object, "OlsSearch"))
    object@rows <- object@numFound
    object@url <- sub("rows=[0-9]+",
                      paste0("rows=", object@numFound),
                      object@url)
    object
}


##########################################
## Coercion

setAs(from = "OlsSearch", to = "data.frame",
      function(from) from@response)

##' @export
as.data.frame.OlsSearch <-
    function(x, row.names = NULL, optional = FALSE, ...) {
        as(x, "data.frame")
    }


##' @export
setAs(from = "OlsSearch", to = "Terms",
      function(from) {
          resp <- from@response
          ## see issue #24
          if (anyNA(resp$obo_id)) {
              i <- is.na(resp$obo_id)
              resp$obo_id[i] <- sub("_", ":", resp$short_form)[i]
          }
          x <- apply(resp, 1,
                     function(x)
                         tryCatch(Term(x[["ontology_prefix"]],
                                       x[["obo_id"]]),
                                  error = function(e) NULL))
          if (is.null(x)) {
              msg <- paste("No result retrieved.",
                           "This might be due to a temporary network issue.")
              stop(msg)
          }
          ## Remove any terms that failed above
          if (any(nullterm <- sapply(x, is.null))) {
              warning(sum(nullterm), " term failed to be instantiated.")
              x <- x[!nullterm]
          }
          names(x) <- resp[["obo_id"]][!nullterm]
          .Terms(x = x)
      })


#########################################
## helper functions

emptyQueryDataFrame <-
    structure(list(id = character(0), iri = character(0),
                   short_form = character(0), obo_id = character(0),
                   label = character(0), description = list(),
                   ontology_name = character(0),
                   ontology_prefix = character(0), type = character(0),
                   is_defining_ontology = logical(0)),
              row.names = integer(0),
              class = "data.frame")
