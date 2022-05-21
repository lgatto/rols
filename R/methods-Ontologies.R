##########################################
## Constructors
setMethod("Ontologies", "missing",
          function() makeOntologies())

setMethod("Ontologies", "numeric",
          function(object) makeOntologies(object))

setMethod("Ontology", "character",
          function(object) {
              url <- ontologyUrl(object)
              x <- GET(url)
              stop_for_status(x)
              cx <- content(x)
              makeOntology(cx)
          })

setMethod("Ontology", "Ontology",
          function(object) object)

##########################################
## show methods

setMethod("show", "Ontology",
          function(object) {
              cat("Ontology: ", olsTitle(object),
                  " (", olsNamespace(object) , ")", sep = "")
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
                  cat("  ", paste(head(olsPrefix(object), n=2), collapse = ", "),
                      "...",
                      paste(tail(olsPrefix(object), n=2), collapse = ", "), "\n")
              else
                  cat(paste(olsPrefix(object)[1:length(object)], collapse = ", "), "\n")
          })

##########################################
## Accessors

setMethod("olsVersion", "character",
          function(object) olsVersion(Ontology(object)))
setMethod("olsVersion", "Ontology",
          function(object) object@config$version)
setMethod("olsVersion", "Ontologies",
          function(object) sapply(object@x, olsVersion))

setMethod("olsLoaded", "character",
          function(object) olsLoaded(Ontology(object)))
setMethod("olsLoaded", "Ontology",
          function(object) substr(object@loaded, 1, 10))
setMethod("olsLoaded", "Ontologies",
          function(object) sapply(object@x, olsLoaded))


setMethod("olsUpdated", "character",
          function(object) olsUpdated(Ontology(object)))
setMethod("olsUpdated", "Ontology",
          function(object) substr(object@updated, 1, 10))
setMethod("olsUpdated", "Ontologies",
          function(object) sapply(object@x, olsUpdated))

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
setMethod("olsRoot", "Ontology",
          function(object) olsRoot(olsPrefix(object)))
setMethod("olsRoot", "Ontologies",
          function(object) lapply(object@x, olsRoot))

setMethod("olsPrefix", "character",
          function(object) olsPrefix(Ontology(object)))
setMethod("olsPrefix", "Ontology",
          function(object) object@config$preferredPrefix)
setMethod("olsPrefix", "Ontologies",
          function(object) sapply(object@x, olsPrefix))

setMethod("olsDesc", "character",
          function(object) olsDesc(Ontology(object)))
setMethod("olsDesc", "Ontology",
          function(object) object@config$description)
setMethod("olsDesc", "Ontologies",
          function(object) sapply(object@x, olsDesc))

setMethod("olsTitle", "character",
          function(object) olsTitle(Ontology(object)))
setMethod("olsTitle", "Ontology",
          function(object) object@config$title)
setMethod("olsTitle", "Ontologies",
          function(object) sapply(object@x, olsTitle))

setMethod("olsStatus", "character",
          function(object) olsStatus(Ontology(object)))
setMethod("olsStatus", "Ontology",
          function(object) object@status)
setMethod("olsStatus", "Ontologies",
          function(object) sapply(object@x, olsStatus))

setMethod("olsNamespace", "character",
          function(object) olsNamespace(Ontology(object)))
setMethod("olsNamespace", "Ontology",
          function(object) object@config$namespace)
setMethod("olsNamespace", "Ontologies",
          function(object) sapply(object@x, olsNamespace))



##########################################
## Data manipulation

setMethod("lapply", "Ontologies",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
setMethod("[", "Ontologies",
          function(x, i, j="missing", drop="missing")
              new("Ontologies", x = x@x[i]))
setMethod("[[", "Ontologies",
          function(x, i, j="missing", drop="missing") x@x[[i]])
setMethod("length", "Ontologies", function(x) length(x@x))


##########################################
## Coercion

setAs("Ontologies", "data.frame",
      function(from) as.data.frame.Ontologies(from))

as.data.frame.Ontologies <- function(x) {
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

setAs("Ontologies", "list",
      function(from) from@x)

setMethod("all.equal", c("Ontologies", "Ontologies"),
          function(target, current) {
              msg <- Biobase::validMsg(NULL, NULL)
              if (length(target) != length(current)) {
                  msg <- Biobase::validMsg(msg, "The 2 Ontologies are of different lengths")
              } else {
                  tg <- target@x
                  ct <- current@x
                  if (any(sort(names(tg)) != sort(names(ct)))) {
                      msg <- validMsg(msg, "Ontology names don't match")
                  } else {
                      ## reorder before comparing Ontolgy objects one
                      ## by one
                      tg <- tg[order(names(tg))]
                      ct <- ct[order(names(ct))]
                      for (i in seq_along(tg)) {
                          eq <- all.equal(tg[[i]], ct[[i]])
                          if (is.character(eq)) {
                              eq <- paste0("Ontology '", names(tg)[i], "': ", eq)
                              msg <- validMsg(msg, eq)
                          }
                      }
                  }
              }
              if (is.null(msg)) return(TRUE)
              else msg
          })

setMethod("all.equal", c("Ontology", "Ontology"),
          function(target, current) {
              msg <- Biobase::validMsg(NULL, NULL)
              sn <- slotNames("Ontology")
              sn0 <- sn[sn != "config"]
              for (i in sn0) {
                  eq <- all.equal(slot(current, i), slot(target, i))
                  if (is.character(eq))
                  msg <- validMsg(msg, paste0(i, ": ", eq))
              }
              c1 <- slot(current, "config")
              c2 <- slot(target, "config")
              c1 <- c1[order(names(c1))]
              c2 <- c2[order(names(c2))]
              msg <- Biobase::validMsg(msg, all.equal(c1, c2))
              if (is.null(msg)) TRUE else msg
          })
