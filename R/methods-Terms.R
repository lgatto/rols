olsLoaded <- function(x) substr(x@loaded, 1, 10)
olsUpdated <- function(x) substr(x@updated, 1, 10)
isObsolete <- function(x) x@is_obsolete
isRoot <- function(x) x@is_root

.termId <- function(x) x@obo_id

makeTerm <- function(x)
    .Term(iri = x$iri,
          label = x$label,
          description = x$description,
          annotation = x$annotation,
          synonym = x$synonym,
          ontology_name = x$ontology_name,
          ontology_prefix = x$ontology_prefix,
          ontology_iri = x$ontology_iri,
          is_obsolete = x$is_obsolete,
          is_defining_ontology = x$is_defining_ontology,
          has_children = x$has_children,
          is_root = x$is_root,
          short_form = x$short_form,
          obo_id = x$obo_id,
          links = x$`_links`)


setMethod("show", "Term",
          function(object) {
              ids <- .termId(object)
              cat("A Term from the", object@ontology_prefix, "ontology:", ids, "\n")
              cat(" Label: ", object@label,"\n  ", sep = "")
              desc <- object@description
              if (is.null(desc)) cat("No description\n")
              else for (i in 1:seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })


setMethod("show", "Terms",
          function(object) {
              cat("Object of class 'Terms' with", length(object), "entries\n")
              if (length(object) > 4)
                  cat("  ", paste(head(termId(object), n=2), collapse = ", "),
                      "...",
                      paste(tail(termId(object), n=2), collapse = ", "), "\n")
              else
                  cat(paste(termId(object)[1:4], collapse = ", "), "\n")
          })

setMethod("length", "Terms", function(x) length(x@x))

setMethod("[", "Terms",
          function(x, i, j="missing", drop="missing") Terms(x = x@x[i]))
          
setMethod("[[", "Terms",
          function(x, i, j="missing", drop="missing") x@x[[i]])



setMethod("termId", "Term", function(object) .termId(object))
setMethod("termId", "Terms",
          function(object) sapply(object@x, .termId))

.terms <- function(goid, pagesize = 200) {
    url <- paste(ontologyUrl(goid), "terms", sep = "/")
    url <- paste0(url, "?&size=", pagesize)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    ## -- Iterating
    .next <- cx[["_links"]][["next"]]$href
    pb <- progress_bar$new(total = cx[["page"]][["totalPages"]])
    while (!is.null(.next)) {
        pb$tick()
        x <- GET(.next)
        warn_for_status(x)
        cx <- content(x)
        ans <- append(ans, lapply(cx[["_embedded"]][[1]], makeTerm))
        .next <- cx[["_links"]][["next"]][[1]]
    }
    cat("\n")
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

.term <- function(goid, termid) {
    url <- paste(ontologyUrl(goid), "terms", sep = "/")
    url <- paste(url, "http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F", sep = "/")
    url <- paste0(url, sub(":", "_", termid))
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    makeTerm(cx)
}

children <- function(id) { ## a Term
    x <- GET(id@links$children[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

parents <- function(id) { ## a Term
    x <- GET(id@links$parents[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

ancestors <- function(id) { ## a Term
    x <- GET(id@links$ancestors[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

descendants <- function(id) { ## a Term
    x <- GET(id@links$descendants[[1]])
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

setMethod("lapply", "Terms",
          function(X, FUN, ...) lapply(X@x, FUN, ...))
