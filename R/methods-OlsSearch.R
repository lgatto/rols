##########################################
## Constructor
OlsSearch <- function(q,
                      ontology = "",
                      type = "",
                      slim = "",
                      fieldList = "",
                      queryFields = "",
                      exact = FALSE,
                      groupField = FALSE,
                      obsoletes = FALSE,
                      local = "",
                      childrenOf = "",
                      rows,
                      start = 0L) {
    if (missing(q))
        stop("You must supply a query.")
    .args <- as.list(match.call())[-1]
    if (missing(rows)) 
        .args[["rows"]] <- rows <- 20L
    ## Create search URL and instantiate OlsSearch object
    params <- c()
    for (i in seq_along(.args)) {
        nm <- names(.args)[i]
        arg <- eval(.args[[i]])
        if (nm == "ontology")
            arg <- tolower(arg)
        if (length(arg) > 1)
            arg <- paste(arg, collapse = ",")
        params <- append(params, paste(nm, arg, sep = "="))
    }
    url <- paste0("http://www.ebi.ac.uk/ols/beta/api/search?",
                  paste(params, collapse = "&"))
    ## Make actual query, with rows = 1 to get the total number of
    ## results found
    url0 <- sub("rows=[0-9]+", "rows=1", url)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    txt <- rawToChar(cx)
    ans <- jsonlite::fromJSON(txt)
    numFound <- ans[["response"]][["numFound"]]
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

olsSearch <- function(object, all = FALSE) {
    if (all)
        x <- allRows(x)
    x <- GET(object@url)
    stop_for_status(x)
    cx <- content(x)
    txt <- rawToChar(cx)
    ans <- jsonlite::fromJSON(txt)
    object@response <- ans[["response"]][["docs"]]
    object
}


##########################################
## show method

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

olsRows <- function(x) {
    stopifnot(inherits(x, "OlsSearch"))
    x@rows
}

"olsRows<-" <- function(x, value) {
    stopifnot(inherits(x, "OlsSearch"))
    x@rows <- as.integer(value)
    x
}

allRows <- function(x) {
    stopifnot(inherits(x, "OlsSearch"))
    x@rows <- x@numFound
    x@url <- sub("rows=[0-9]+", paste0("rows=", x@numFound), x@url)
    x
}


##########################################
## Coercion

as.data.frame.OlsSearch <-
    function(x) x@response

setAs(from = "OlsSearch", to = "data.frame",
      function(from) from@response)

## Terms constructor
setAs(from = "OlsSearch", to = "Terms",
      function(from) {
          x <- apply(from@response, 1,
                     function(x) term(x[["ontology_prefix"]],
                                      x[["obo_id"]]))
          Terms(x = x)
      })
