test_that("ontologyUri", {
    go <- Ontology("go")
    expect_identical(rols:::ontologyUri(go, encode = FALSE),
                     rols:::ontologyUri(encode = FALSE))
    expect_identical(rols:::ontologyUri(go, encode = TRUE),
                     rols:::ontologyUri(encode = TRUE))
    ## not in this case - https://github.com/EBISPOT/OLS/issues/35
    ordo <- Ontology("ordo")
    expect_false(rols:::ontologyUri(ordo, encode = FALSE) == rols:::ontologyUri(encode = FALSE))
    expect_equal(rols:::ontologyUri(ordo, encode = FALSE),
                 "http://www.orpha.net/ORDO/")
})



test_that("ontologyUri with multiple URIs", {
    go0 <- go <- Ontology("go")
    go@config$baseUris[[2]] <- "foo"
    expect_warning(rols:::ontologyUri(go, encode = FALSE))
    expect_equal(rols:::ontologyUri(go, encode = FALSE),
                 rols:::ontologyUri(go0, encode = FALSE))
})
