##########################################
## Constructors

## These methods query an Ontology (or its prefix) for all or one term
setMethod("terms", "character",
          function(x, ...) .terms(object, ...))
setMethod("terms", "Ontology",
          function(x, ...) .terms(olsPrefix(object), ...))
setMethod("term", c("character", "character"),
          function(object, id, ...) .term(object, id, ...))
setMethod("term", c("Ontology", "character"),
          function(object, id,...) .term(olsPrefix(object), id, ...))


children <- function(id) {
    stopifnot(inherits(id, "Term"))
    x <- GET(id@links$children[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

parents <- function(id) {
    stopifnot(inherits(id, "Term"))
    x <- GET(id@links$parents[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

ancestors <- function(id) {
    stopifnot(inherits(id, "Term"))
    x <- GET(id@links$ancestors[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

descendants <- function(id) {
    stopifnot(inherits(id, "Term"))
    x <- GET(id@links$descendants[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

##########################################
## show methods

setMethod("show", "Term",
          function(object) {
              ids <- .termId(object)
              cat("A Term from the", object@ontology_prefix, "ontology:", ids, "\n")
              cat(" Label: ", olsLabel(object),"\n  ", sep = "")
              desc <- object@description
              if (is.null(desc)) cat("No description\n")
              else for (i in 1:seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })


setMethod("show", "Terms",
          function(object) {
              cat("Object of class 'Terms' with", length(object), "entries\n")
              onts <- unique(olsPrefix(object))
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

isObsolete <- function(x) {
    stopifnot(inherits(x, "Term"))
    x@is_obsolete
}

isRoot <- function(x) {
    stopifnot(inherits(x, "Term"))
    x@is_root
}

olsSynonym <- function(x) {
    stopifnot(inherits(x, "Term"))
    unlist(x@synonym)
}

setMethod("olsLabel", "Term",
          function(object) object@label)

setMethod("olsLabel", "Terms",
          function(object) sapply(object@x, olsLabel))

setMethod("termId", "Term",
          function(object) .termId(object))

setMethod("termId", "Terms",
          function(object) sapply(object@x, .termId))

setMethod("olsPrefix", "Term",
          function(object) object@ontology_prefix)

setMethod("olsPrefix", "Terms",
          function(object) sapply(object@x, olsPrefix))


##########################################
## Data manipulation

setMethod("length", "Terms", function(x) length(x@x))

setMethod("[", "Terms",
          function(x, i, j="missing", drop="missing") Terms(x = x@x[i]))
          
setMethod("[[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[[i]])

setMethod("lapply", "Terms",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
