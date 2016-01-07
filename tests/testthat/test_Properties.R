context("Property/ies")

test_that("Property constructors", {
    soprops <- properties("so")
    ont <- Ontology("so")
    soprops2 <- properties(ont)
    ## note that this is true only of the order of the two requests is
    ## the same. This seems to be the case here, pronably because
    ## there are relatively rew results. Might need to implement an
    ## all.equal method though.
    expect_identical(soprops, soprops2)

    trm <- term("uberon", "UBERON_0002107")
    p <- properties(trm)
    expect_is(p, "Properties")
    expect_identical(length(p), 11L)

    expect_identical(termId(p), sapply(p@x, "slot", "obo_id"))

    trm2 <- term("uberon", "UBERON_0002108")
    trm3 <- term("uberon", "UBERON_0000002")

    trmlst <- c(trm, trm2, trm3)
    names(trmlst) <- sapply(trmlst, termId)
    trms <- rols:::Terms(x = trmlst)
    
    pl <- properties(trms)
    expect_is(pl, "list")
    expect_identical(length(pl), 3L)


    k <- c("SO:0000579", "SO:0000833", "SO:0000578", "SO:0000011",
           "SO:0000577", "SO:0000628", "SO:0001431", "SO:0000704",
           "SO:0000976", "SO:0000576")
    
    pl <- properties(so[k])

    expect_message(x <- properties(so[k[1]]), "No properties for term SO:0000579")
    expect_null(x[[1]])
    x <- properties(so[k])
    expect_identical(sum(sapply(x, is.null)), 7L)
})


test_that("Property/ies show", {
    expect_null(show(properties("so")))
    trm <- term("uberon", "UBERON_0002107")
    p <- properties(trm)
    expect_null(show(p))
})
