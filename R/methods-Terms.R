##########################################
## Constructors

## Using the ontologyId
setMethod("Terms", "character",
          function(x, pagesize = 1000)
              makeTerms(x, pagesize))

## Using an Ontology instance
setMethod("Terms", "Ontology",
          function(x, pagesize = 1000)
              makeTerms(x, pagesize))


##' @title Constructs the query for all term from a given ontology
##'
##' @param oid `character(1)` with an ontologyIf or an `Ontology`
##'     instance.
##'
##' @param pagesize `numerci(1)` indicating the number of results per
##'     page to return. Default is `1000`.
##'
##' @return An object of class Terms
##'
makeTerms <- function(oid, pagesize) {
    ont <- Ontology(oid)
    url <- paste0(ont@links$terms$href)
    ## url <- paste(ontologyUrl(ont), "terms", sep = "/")
    url <- paste0(url, "?&size=", as.integer(pagesize))
    x <- lapply(
        req_perform_iterative(
            request(url),
            next_req,
            progress = TRUE),
        resp_embedded,
        what = "terms") |>
        unlist(recursive = FALSE)
    ans <- lapply(x, termFromJson)
    names(ans) <- sapply(ans, termId)
    .Terms(x = ans)
}

## These methods query an Ontology (or its prefix) for all or one term
## setMethod("terms", "character",
##           function(x, ...) .terms(x, ...))
## setMethod("terms", "Ontology",
##           function(x, ...) .terms(olsNamespace(x), ...))
## setMethod("term", c("character", "character"),
##           function(object, id, ...) .term(object, id, ...))
## setMethod("term", c("Ontology", "character"),
##           function(object, id,...) .term(object, id, ...))


partOf <- function(id) {
    stopifnot(inherits(id, "Term"))
    url <- id@links$part_of[[1]]
    if (is.null(url)) {
        message("No 'part of' terms.")
        return(NULL)
    }
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

derivesFrom <- function(id) {
    stopifnot(inherits(id, "Term"))
    url <- id@links$derives_from[[1]]
    if (is.null(url)) {
        message("No 'derives from' terms.")
        return(NULL)
    }
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

children <- function(id) {
    pagesize <- 20
    stopifnot(inherits(id, "Term"))
    url0 <- id@links$children[[1]]
    if (is.null(url0)) {
        message("No children terms.")
        return(NULL)
    }
    url <- paste0(url0, "?page=0&size=", pagesize)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    if (cx$page$totalElements > pagesize) {
        pagesize <- cx$page$totalElements
        url <- paste0(url0, "?page=0&size=", pagesize)
        x <- GET(url)
        warn_for_status(x)
        cx <- content(x)
    }
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

parents <- function(id) {
    pagesize <- 20
    stopifnot(inherits(id, "Term"))
    url0 <- id@links$parents[[1]]
    if (is.null(url0)) {
        message("No parent terms.")
        return(NULL)
    }
    url <- paste0(url0, "?page=0&size=", pagesize)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    if (cx$page$totalElements > pagesize) {
        pagesize <- cx$page$totalElements
        url <- paste0(url0, "?page=0&size=", pagesize)
        x <- GET(url)
        warn_for_status(x)
        cx <- content(x)
    }
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

ancestors <- function(id) {
    pagesize <- 20
    stopifnot(inherits(id, "Term"))
    url0 <- id@links$ancestors[[1]]
    if (is.null(url0)) {
        message("No ancestor terms.")
        return(NULL)
    }
    url <- paste0(url0, "?page=0&size=", pagesize)
    x <- GET(url)
    stop_for_status(x)
    cx <- content(x)
    if (cx$page$totalElements > pagesize) {
        pagesize <- cx$page$totalElements
        url <- paste0(url0, "?page=0&size=", pagesize)
        x <- GET(url)
        warn_for_status(x)
        cx <- content(x)
    }
    ans <- lapply(cx[["_embedded"]][[1]], makeTerm)
    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

descendants <- function(id) {
    pagesize <- 20
    stopifnot(inherits(id, "Term"))
    url0 <- id@links$descendants[[1]]
    if (is.null(url0)) {
      message("No descendant terms.")
      return(NULL)
    }
    url <- paste0(url0, "?page=0&size=", pagesize)
    x <- httr::GET(url)
    stop_for_status(x)
    cx <- content(x)
    if(cx$page$totalElements > pagesize){
      pagesize <- cx$page$totalElements

      #Figure out how many pages the results will divide into.
      url_pages <- c(0:(ceiling(pagesize/1000)-1))

      #Make as many URLs as there are pages anticipated.
      url <- paste0(url0, "?page=",url_pages,"&size=", pagesize)

      #Loop through queries of each URL.
      cx  <- lapply(url, function(x) {
        a <- GET(x)
        warn_for_status(a)
        a <- content(a)})
    }else{
      #Put singular <cx> into a list for lapply()'s sake.
      cx  <- list(cx)
    }
    ans   <- lapply(cx, function(x)
      lapply(x[["_embedded"]][[1]], makeTerm))

    #Concatenate all <cx> results into one list.
    ans   <- do.call(c,ans)

    names(ans) <- sapply(ans, termId)
    Terms(x = ans)
}

##########################################
## show methods

setMethod("show", "Term",
          function(object) {
              ids <- .termId(object)
              cat("A Term from the", termPrefix(object), "ontology:", ids, "\n")
              cat(" Label: ", termLabel(object),"\n  ", sep = "")
              desc <- termDesc(object)
              if (is.null(desc)) cat("No description\n")
              else for (i in 1:seq_along(desc))
                  cat(strwrap(desc[[i]]), sep = "\n  ")
          })


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

setMethod("termSynonym", "Term",
          function(object) unlist(object@synonyms))
setMethod("termSynonym", "Terms",
          function(object) lapply(object@x, termSynonym))

setMethod("isObsolete", "Term",
          function(object) object@is_obsolete)
setMethod("isObsolete", "Terms",
          function(object) sapply(object@x, isObsolete))

setMethod("isRoot", "Term",
          function(object) object@is_root)
setMethod("isRoot", "Terms",
          function(object) sapply(object@x, isRoot))

setMethod("termLabel", "Term",
          function(object) object@label)
setMethod("termLabel", "Terms",
          function(object) sapply(object@x, termLabel))

setMethod("termId", "Term",
          function(object) .termId(object))
setMethod("termId", "Terms",
          function(object) sapply(object@x, .termId))

setMethod("termPrefix", "Term",
          function(object) object@ontology_prefix)
setMethod("termPrefix", "Terms",
          function(object) sapply(object@x, termPrefix))

setMethod("termDesc", "Term",
          function(object) unlist(object@description))
setMethod("termDesc", "Terms",
          function(object) sapply(object@x, termDesc))

setMethod("termOntology", "Term",
          function(object) unlist(object@ontology_name))
setMethod("termOntology", "Terms",
          function(object) sapply(object@x, termOntology))

setMethod("termNamespace", "Term",
          function(object) unlist(object@annotation$has_obo_namespace))
setMethod("termNamespace", "Terms",
          function(object) sapply(object@x, termNamespace))

##########################################
## Data manipulation

setMethod("length", "Terms", function(x) length(x@x))

## setMethod("unique", "Terms", function(x) x[!duplicated(names(x@x))])

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

fix_null <- function(x) {
    if (is.null(x)) return(NA)
    if (is.list(x)) return(x[[1]])
    return(x)
}

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

as.Term.data.frame <- function(x)
    as(x, "data.frame")

setAs("Terms", "data.frame",
      function(from) do.call(rbind, lapply(from, as, "data.frame")))

as.Terms.data.frame <- function(x)
    as(x, "data.frame")

#############################################
## utils
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
