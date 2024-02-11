test_that("Property constructors", {
    soprops <- Properties("so")
    ont <- Ontology("so")
    soprops2 <- Properties(ont)
    ## note that this is true only of the order of the two requests is
    ## the same. This seems to be the case here, probably because
    ## there are relatively rew results. Might need to implement an
    ## all.equal method though.
    expect_identical(soprops, soprops2)

    trm <- Term("uberon", "UBERON_0002107")
    p <- Properties(trm)
    expect_is(p, "Properties")

    expect_identical(termId(p), sapply(p@x, "slot", "obo_id"))

    trm2 <- Term("uberon", "UBERON_0002108")
    trm3 <- Term("uberon", "UBERON_0000002")

    trmlst <- c(trm, trm2, trm3)
    names(trmlst) <- sapply(trmlst, termId)
    trms <- rols:::.Terms(x = trmlst)

    pl <- Properties(trms)
})


test_that("Property/ies show", {
    expect_null(show(Properties("so")))
    trm <- Term("uberon", "UBERON_0002107")
    p <- Properties(trm)
    expect_null(show(p))
    expect_null(show(p[[1]]))
})
