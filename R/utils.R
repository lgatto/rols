#####################################
## httr2 utils
##' @import httr2
next_req <- function(resp, req) {
    .next <- resp_body_json(resp)[["_links"]][["next"]]$href
    if (is.null(.next))
        return(NULL)
    request(.next)
}

resp_embedded <- function(resp, what,
                          embedded = "_embedded") {
    body <- resp_body_json(resp)
    body[[embedded]][[what]]
}

##' @param url `character(1)` with the request URL.
##'
##' @param what `character(1)` defining the embedded slot name,
##'     typically `"ontologies"` for `Ontologies()` or `"terms"` for
##'     `Terms()`. Passed to `resp_embedded()`.
##'
##' @param embedded `character(1)` defining where to find the embedded
##'     slot name `what`. Default is `"_embedded"`, which work for
##'     ontologies and terms, but should be set to `"response"` for an
##'     OLS query.
##'
##' @return A `list()` responsons in json format that can be converted
##'     into ontology or term objects with the `ontologyFromJson()`
##'     and `termFromJson()` functions.  See `makeOntologies()` and
##'     `makeTerms()`, `parents()`, ... for examples.
##'
##' @noRd
ols_requests <- function(url, what,
                         embedded = "_embedded")
    lapply(
        req_perform_iterative(
            request(url),
            next_req,
            max_reqs = Inf,
            progress = TRUE),
        resp_embedded,
        what = what) |>
        unlist(recursive = FALSE)
