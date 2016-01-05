## a test to check the ontology URIs

test_uris <- function(ol) {
    library(rols)
    msg <- Biobase::validMsg(NULL, NULL)
    ## ol <- Ontologies()
    uris <- lapply(ol@x, function(xx) xx@config$baseUris)
    if (!all(lengths(uris) == 1))
        msg <- Biobase::validMsg("Some length(URIs) != 1", msg)
    if (any(unlist(sapply(uris, is.null))))
        msg <- Biobase::validMsg("Some URIs are NULL", msg)
    if (any(unlist(sapply(uris, is.na))))
        msg <- Biobase::validMsg("Some URIs are NA", msg)
    if (is.null(msg)) return(TRUE)
    else return(msg)
}
