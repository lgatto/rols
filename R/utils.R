#####################################
## httr2 utils
##' @import httr2
next_req <- function(resp, req) {
    .next <- resp_body_json(resp)[["_links"]][["next"]]$href
    if (is.null(.next))
        return(NULL)
    request(.next)
}

resp_embedded <- function(resp, what) {
    body <- resp_body_json(resp)
    body[["_embedded"]][[what]]
}

ols_requests <- function(url, what)
    lapply(
        req_perform_iterative(
            request(url),
            next_req,
            max_reqs = Inf,
            progress = TRUE),
        resp_embedded,
        what = what) |>
        unlist(recursive = FALSE)
