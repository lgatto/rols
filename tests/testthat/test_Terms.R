go <- Ontology("GO")
trm <- Term(go, "GO:0032801")
trms <- Terms("SO", pagesize = 1000)

test_that("constructors", {
    ## so <- Ontology("SO")
    ## trms <- Terms("SO")
    ## trms2 <- Terms(so)
    ## expect_true(rols:::all.equal(trms, trms2))
    ## expect_identical(length(trms[1:10]), 10L)

    go <- Ontology("GO")
    trm <- Term(go, "GO:0032801")
    trm1 <- Term(go, "GO:0032801")
    trm2 <- Term("go", "GO:0032801")
    trm3 <- Term("GO", "GO:0032801")
    expect_identical(trm1, trm2)
    expect_identical(trm1, trm3)

    expect_identical(termPrefix(trm), "GO")
    expect_identical(termLabel(trm), "receptor catabolic process")
    expect_identical(termId(trm), "GO:0032801")

    trm1 <- trms[["SO:1000005"]]
    trm2 <- Term("SO", "SO:1000005")
    expect_identical(trm1, trm2)
    expect_identical(termPrefix(trm1), "SO")
    expect_true(all(termPrefix(trms) == "SO"))

    expect_identical(termPrefix(trm), "GO")
    expect_identical(termLabel(trm), "receptor catabolic process")

    nms <- names(descendants(trm)@x)
    expect_identical(children(trm)@x[nms],
                     descendants(trm)@x[nms])
})

test_that("constructors different URIs (issue 42)", {
    ## See https://github.com/lgatto/rols/issues/42
    expect_is(Term("ado", "ADO:0000090"), "Term")
    expect_is(Term("efo", "EFO:0001200"), "Term")
    expect_is(Term("go", "GO:0005802"), "Term")
})

test_that("show methods", {
    expect_null(show(trms))
    ## expect_null(show(trms[1]))
    ## expect_null(show(trms[1:2]))
    ## expect_null(show(trms[1:3]))
    ## expect_null(show(trms[1:4]))
    ## expect_null(show(trms[1:5]))
    expect_null(show(trm))
})

test_that("accessors", {
    ## expect_identical(length(termSynonym(trms[1:2])), 2L)
    expect_false(isObsolete(trm))
    expect_true(isObsolete(Term("GO", "GO:0005563")))
    expect_false(isObsolete(Term("GO", "GO:0030533")))

    expect_true(isRoot(trms[["SO:0000400"]]))


    expect_identical(length(termDesc(trms)), length(trms))
    expect_identical(length(termLabel(trms)), length(trms))

    expect_true(all(unlist(termNamespace(trms)) == "sequence"))
    expect_identical(termNamespace(trm), "biological_process")

    expect_true(all(unlist(termOntology(trms)) == "so"))
    expect_identical(termOntology(trm), "go")
})

test_that("apply over Terms", {
    expect_identical(unlist(lapply(trms, termId)),
                     termId(trms))
})


test_that("terms(pagesize)", {
    trms1 <- Terms("SO", pagesize = 20, obsolete = TRUE)
    trms2 <- Terms("SO", pagesize = length(trms1), obsolete = TRUE)
    trms3 <- Terms("SO", pagesize = 1000, obsolete = TRUE) ## > length(trms1)
    expect_true(all.equal(trms1, trms2))
    expect_true(all.equal(trms1, trms3))
})

test_that("No links", {
    trm <- Term("GO", "GO:0030232")
    ## does not have any children
    expect_null(children(trm))
    ## does not have any descendants
    expect_null(descendants(trm))
    ## does have parents and ancestors, though
    expect_is(parents(trm), "Terms")
    expect_is(ancestors(trm), "Terms")
})
