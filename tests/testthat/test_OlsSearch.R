context("OlsSearch")

## test_that("OlsSearch tgn", {
##     tgn <- OlsSearch(q = "tgn",ontology = "GO")
##     tgn <- olsSearch(tgn)

##     tgnres <- structure(c("trans-Golgi network",
##                           "clathrin coat of trans-Golgi network vesicle",
##                           "Golgi to endosome transport",
##                           "trans-Golgi network transport vesicle",
##                           "trans-Golgi network transport vesicle membrane"),
##                         .Names = c("GO:0005802", "GO:0030130",
##                                    "GO:0006895", "GO:0030140", "GO:0012510"))

##     expect_identical(sort(termLabel(as(tgn, "Terms"))),
##                      sort(tgnres))
## })


## test_that("OlsSearch ESI", {

##     esi2 <- OlsSearch(q = "ESI", ontology = "MS", rows = 28)
##     esi1 <- OlsSearch(q = "ESI", ontology = "MS", exact = TRUE)

##     esi1 <- olsSearch(esi1)
##     esi2 <- olsSearch(esi2)

##     expect_identical(esi1@numFound, 1L)
##     expect_identical(esi2@numFound, 34L)
##     expect_true(termId(as(esi1, "Terms")) %in% termId(as(esi2, "Terms")))
## })


test_that("OlsSearch tgn", {
    tgnpw <- OlsSearch("tgn","PW")
    tgnpw <- olsSearch(allRows(tgnpw))
    expect_equal(tgnpw@numFound, 4L)

    tgn <- OlsSearch("tgn")
    tgn <- olsSearch(allRows(tgn))

    expect_true(all(tgnpw@response[, "obo_id"] %in% tgn@response[, "obo_id"]))
})

test_that("OlsSearch show",
    expect_null(show(OlsSearch(q = "cell", ontology = "GO"))))

test_that("OlsSearch rows", {
    res <- OlsSearch(q = "cell", ontology = "GO")
    expect_equal(res@rows, 20L)
    expect_equal(nrow(olsSearch(res)@response), 20L)

    res <- OlsSearch(q = "cell", ontology = "GO", rows = 32)
    expect_equal(nrow(olsSearch(res)@response), 32L)
    expect_equal(res@rows, 32L)
    expect_equal(res@rows, olsRows(res))

    olsRows(res) <- 108
    expect_equal(olsRows(res), 108L)
    expect_equal(nrow(olsSearch(res)@response), 108L)

    res <- allRows(res)
    expect_equal(olsRows(res), res@numFound)
    expect_equal(nrow(olsSearch(res)@response), res@numFound)
})

test_that("OlsSearch coercion", {
    res <- OlsSearch(q = "plasma", ontology = "GO", rows = 32)
    res <- olsSearch(res)

    resdf <- as(res, "data.frame")
    expect_true(class(resdf) == "data.frame")

    resterms <- as(res, "Terms")
    expect_true(class(resterms) == "Terms")

    expect_identical(nrow(resdf), length(resterms))
    expect_equivalent(termId(resterms), resdf[, "obo_id"])
    expect_equivalent(termLabel(resterms), resdf[, "label"])
})

test_that("Empty olsSearch query", {
    qry <- OlsSearch("asdasdasdasd", ontology = "CL")
    ## used to fail with error
    out <- olsSearch(qry)
    expect_true(validObject(out))
    out <- as(out, "data.frame")
    expect_identical(nrow(out), 0L)
    expect_identical(ncol(out), 10L)
})
