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

    res <- olsSearch(res) ## max is 1000
    expect_equal(nrow(res@response), 1000)
})

test_that("OlsSearch coercion", {
    res <- OlsSearch(q = "plasma", ontology = "GO", rows = 32)
    res <- olsSearch(res)

    resdf <- as(res, "data.frame")
    expect_true(class(resdf) == "data.frame")

    resterms <- as(res, "olsTerms")
    expect_is(resterms, "olsTerms")

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
