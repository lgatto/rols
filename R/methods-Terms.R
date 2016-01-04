##########################################
## Constructors

## These methods query an Ontology (or its prefix) for all or one term
setMethod("terms", "character",
          function(x, ...) .terms(x, ...))
setMethod("terms", "Ontology",
          function(x, ...) .terms(olsNamespace(x), ...))

setMethod("term", c("character", "character"),
          function(object, id, ...) .term(object, id, ...))
setMethod("term", c("Ontology", "character"),
          function(object, id,...) .term(object, id, ...))


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
              cat("A Term from the", olsPrefix(object), "ontology:", ids, "\n")
              cat(" Label: ", olsLabel(object),"\n  ", sep = "")
              desc <- olsDesc(object)
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

setMethod("olsSynonym", "Term",
          function(object) unlist(object@synonym))
setMethod("olsSynonym", "Terms",
          function(object) sapply(object@x, olsSynonym))


setMethod("isObsolete", "Term",
          function(object) object@is_obsolete)
setMethod("isObsolete", "Terms",
          function(object) sapply(object@x, isObsolete))

setMethod("isRoot", "Term",
          function(object) object@is_root)
setMethod("isRoot", "Terms",
          function(object) sapply(object@x, isRoot))

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

setMethod("olsDesc", "Term",
          function(object) unlist(object@description))
setMethod("olsDesc", "Terms",
          function(object) sapply(object@x, olsDesc))

setMethod("olsNamespace", "Term",
          function(object) unlist(object@annotation$has_obo_namespace))
setMethod("olsNamespace", "Terms",
          function(object) sapply(object@x, olsNamespace))

##########################################
## Data manipulation

setMethod("length", "Terms", function(x) length(x@x))

setMethod("[", "Terms",
          function(x, i, j="missing", drop="missing") Terms(x = x@x[i]))
          
setMethod("[[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[[i]])

setMethod("lapply", "Terms",
          function(X, FUN, ...) lapply(X@x, FUN, ...))

setMethod("all.equal", c("Term", "Term"),
          function(target, current) {
              msg <- Biobase::validMsg(NULL, NULL)
              snms <- slotNames("Term")
              for (i in snms[-grep("links", snms)]) {
                  eq <- all.equal(slot(target, i), slot(current, i))
                  if (is.character(eq)) {
                      eq <- paste0("Slot '", i, "': ", eq)
                      msg <- Biobase:::validMsg(msg, eq)
                  }
              }
              lt <- slot(target, "links")
              lc <- slot(current, "links")
              ot <- order(names(lt))
              oc <- order(names(lc))
              msg <- Biobase:::validMsg(msg, all.equal(lt[ot], lc[oc]))
              if (is.null(msg)) return(TRUE)
              else msg                  
          })


setMethod("all.equal", c("Terms", "Terms"),
          function(target, current) {
              msg <- Biobase::validMsg(NULL, NULL)
              if (length(target) != length(current)) {
                  msg <- Biobase::validMsg(msg, "2 Terms are of different lengths")
              } else {
                  tg <- target@x
                  ct <- current@x
                  if (any(sort(names(tg)) != sort(names(ct)))) {
                      msg <- Biobase::validMsg(msg, "Term ids don't match")
                  } else {             
                      ot <- order(names(tg))
                      oc <- order(names(ct))
                      tg <- tg[ot]
                      ct <- ct[oc]
                      for (i in seq_along(tg)) {
                          eq <- all.equal(tg[[i]], ct[[i]])
                          if (is.character(eq)) {
                              eq <- paste0("Term id '", names(tg)[i], "': ", eq)
                              msg <- Biobase:::validMsg(msg, eq)
                          }
                      }
                  }
              }
              if (is.null(msg)) return(TRUE)
              else msg
          })
