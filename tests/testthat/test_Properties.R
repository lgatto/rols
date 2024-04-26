test_that("Property constructors", {
    soprops <- olsProperties("so")
    ont <- olsOntology("so")
    soprops2 <- olsProperties(ont)
    ## note that this is true only of the order of the two requests is
    ## the same. This seems to be the case here, probably because
    ## there are relatively rew results. Might need to implement an
    ## all.equal method though.
    expect_identical(soprops, soprops2)

    trm <- olsTerm("uberon", "UBERON_0002107")
    p <- olsProperties(trm)
    expect_is(p, "olsProperties")

    expect_identical(termId(p), sapply(p@x, "slot", "obo_id"))

    trm2 <- olsTerm("uberon", "UBERON_0002108")
    trm3 <- olsTerm("uberon", "UBERON_0000002")

    trmlst <- c(trm, trm2, trm3)
    names(trmlst) <- sapply(trmlst, termId)
    trms <- rols:::.olsTerms(x = trmlst)

    pl <- olsProperties(trms)
})


test_that("Property/ies show", {
    expect_null(show(olsProperties("so")))
    trm <- olsTerm("uberon", "UBERON_0002107")
    p <- olsProperties(trm)
    expect_null(show(p))
    expect_null(show(p[[1]]))
})
