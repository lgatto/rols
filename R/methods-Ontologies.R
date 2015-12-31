##########################################
## Constructors
setMethod("Ontologies", "missing",
          function() makeOntologies())

setMethod("Ontologies", "Ontologies",
          function(object) object@x)

setMethod("Ontology", "character",
          function(object) {
              url <- ontologyUrl(object)
              x <- GET(url)
              stop_for_status(x)
              cx <- content(x)
              makeOntology(cx)
          })

##########################################
## show methods

setMethod("show", "Ontology",
          function(object) {
              cat("Ontology: ", olsTitle(object),
                  " (", olsPrefix(object) , ")", sep = "")
              cat("  ", strwrap(olsDesc(object)), sep = "\n  ")
              cat("   Loaded:", olsLoaded(object),
                  "Updated:", olsUpdated(object),
                  "Version:", olsVersion(object), "\n")
              cat("  ", object@numberOfTerms, "terms ", 
                  object@numberOfProperties, "properties ",
                  object@numberOfIndividuals, "individuals\n")
          })

setMethod("show", "Ontologies",
          function(object) {
              cat("Object of class 'Ontologies' with", length(object), "entries\n")
              if (length(object) > 4)
                  cat("  ", paste(head(olsPrefix(object), n=3), collapse = ", "),
                      "...",
                      paste(tail(olsPrefix(object), n=3), collapse = ", "), "\n")
              else
                  cat(paste(olsPrefix(object)[1:4], collapse = ", "), "\n")
          })


##########################################
## Accessors

olsVersion <- function(x) {
    stopifnot(inherits(x, "Ontology"))
    x@config$version
}

olsLoaded <- function(x) {
    stopifnot(inherits(x, "Ontology"))
    substr(x@loaded, 1, 10)
}

olsUpdated <- function(x) {
    stopifnot(inherits(x, "Ontology"))
    substr(x@updated, 1, 10)
}

setMethod("olsRoot", "Ontology",
          function(object) olsRoot(olsPrefix(object)))

setMethod("olsRoot", "character",
          function(object) {
              url <- ontologyUrl(object)
              url <- paste0(url, "/terms/roots")
              x <- GET(url)
              stop_for_status(x)
              cx <- content(x)
              ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
              names(ans) <- sapply(ans, termId)
              Terms(x = ans)
          })

setMethod("olsPrefix", "Ontology",
          function(object) object@config$preferredPrefix)
setMethod("olsPrefix", "Ontologies",
          function(object) sapply(object@x, olsPrefix))

setMethod("olsDesc", "Ontology",
          function(object) object@config$description)
setMethod("olsDesc", "Ontologies",
          function(object) sapply(object@x, olsDesc))

setMethod("olsTitle", "Ontology",
          function(object) object@config$title)
setMethod("olsTitle", "Ontologies",
          function(object) sapply(object@x, olsTitle))


##########################################
## Data manipulation

setMethod("lapply", "Ontologies",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
setMethod("[", "Ontologies",
          function(x, i, j="missing", drop="missing") Ontologies(x = x@x[i]))
setMethod("[[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[[i]])
setMethod("length", "Ontologies", function(x) length(x@x))


##########################################
## Coercion

setAs("Ontologies", "data.frame",
      function(from) as.data.frame.Ontologies(from))

as.data.frame.Ontologies <- function(x)
    data.frame(Prefix = olsPrefix(x),
               Title = olsTitle(x))


