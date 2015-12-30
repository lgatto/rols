olsVersion <- function(x) x@config$version

.getOntologies <- function() {
    n <- 150
    x <- GET(paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies?page=0&size=", n))
    warn_for_status(x)
    cx <- content(x)
    if (cx$page$totalElements > n) {
        n <- cx$page$totalElements
        x <- GET(paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies?page=0&size=", n))
        warn_for_status(x)
        cx <- content(x)
    }        
    ans <- lapply(cx[["_embedded"]][[1]], .makeOntology)
    names(ans) <- sapply(ans, olsPrefix)
    Ontologies(x = ans)
}


setMethod("olsRoot", "Ontology", function(object) olsRoot(olsPrefix(object)))

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

ontologyUrl <- function(goid)
    paste0("http://www.ebi.ac.uk/ols/beta/api/ontologies/", goid)

Ontology <- function(x) {
    url <- ontologyUrl(x)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    .makeOntology(cx)
}

.makeOntology <- function(x)
    .Ontology(loaded = x$loaded,
              updated = x$updated,
              status = x$status,
              message = x$message,
              version = x$version,
              numberOfTerms = x$numberOfTerms,
              numberOfProperties = x$numberOfProperties,
              numberOfIndividuals = x$numberOfIndividuals,
              config = x$config)



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


setMethod("length", "Ontologies", function(x) length(x@x))

setMethod("olsPrefix", "Ontology", function(object) object@config$preferredPrefix)
setMethod("olsPrefix", "Ontologies", function(object) sapply(object@x, olsPrefix))
setMethod("olsDesc", "Ontology", function(object) object@config$description)
setMethod("olsDesc", "Ontologies", function(object) sapply(object@x, olsDesc))
setMethod("olsTitle", "Ontology", function(object) object@config$title)
setMethod("olsTitle", "Ontologies", function(object) sapply(object@x, olsTitle))

setMethod("ontologies", "missing", .getOntologies)

setMethod("ontologies", "Ontologies", function(object) object@x)

setMethod("[", "Ontologies",
          function(x, i, j="missing", drop="missing") Ontologies(x = x@x[i]))
          
setMethod("[[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[[i]])

setAs("Ontologies", "data.frame",
      function(from) as.data.frame.Ontologies(from))

as.data.frame.Ontologies <- function(x)
    data.frame(Prefix = olsPrefix(x),
               Title = olsTitle(x))


setMethod("terms", "character", function(object, ...) .terms(object, ...))
setMethod("terms", "Ontology", function(object, ...) .terms(olsPrefix(object), ...))


setMethod("term", c("character", "character"),
          function(object, id, ...) .term(object, id, ...))
setMethod("term", c("Ontology", "character"),
          function(object, id,...) .term(olsPrefix(object), id, ...))

setMethod("lapply", "Ontologies",
          function(X, FUN, ...) lapply(X@x, FUN, ...))

