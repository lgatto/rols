context("Term/Terms")

trm <- term(go, "GO:0032801")
trms <- terms("SO", pagesize = 1000)

test_that("constructors", {
    trms <- terms("SO")
    trms2 <- terms(so)
    expect_true(all.equal(trms, trms2))
    expect_identical(length(trms[1:10]), 10L)
    
    trm <- term(go, "GO:0032801")
    go <- Ontology("GO")
    trm1 <- term(go, "GO:0032801")
    trm2 <- term("go", "GO:0032801")
    trm3 <- term("GO", "GO:0032801")
    expect_identical(trm1, trm2)
    expect_identical(trm1, trm3)

    expect_identical(olsPrefix(trm), "GO")
    expect_identical(olsLabel(trm), "receptor catabolic process")
    expect_identical(termId(trm), "GO:0032801")


    trm1 <- trms[["SO:1000005"]]
    trm2 <- term("SO", "SO:1000005")
    expect_identical(trm1, trm2)
    expect_identical(olsPrefix(trm1), "SO")
    expect_true(all(olsPrefix(trms) == "SO"))

    expect_identical(olsPrefix(trm), "GO")
    
    expect_identical(olsSynonym(trm),
                     c("receptor breakdown",
                       "receptor degradation",
                       "receptor catabolism"))

    expect_identical(olsDesc(trm),
                     "The chemical reactions and pathways resulting in the breakdown of a receptor molecule, a macromolecule that undergoes combination with a hormone, neurotransmitter, drug or intracellular messenger to initiate a change in cell function.")

    expect_identical(olsLabel(trm), "receptor catabolic process")

    expect_identical(sort(olsLabel(children(trm))),
                     sort(c('GO:0038018' = "Wnt receptor catabolic process",
                            'GO:1990172' = "G-protein coupled receptor catabolic process",
                            'GO:0032802' = "low-density lipoprotein particle receptor catabolic process",
                            'GO:0097019' = "neurotransmitter receptor catabolic process")))


    expect_identical(sort(olsLabel(parents(trm))),
                     sort(c('GO:0043112' = "receptor metabolic process",
                            'GO:0044248' = "cellular catabolic process",
                            'GO:0009057' = "macromolecule catabolic process")))

    expect_identical(length(ancestors(trm)), 20L)

    nms <- names(descendants(trm)@x)
    expect_identical(children(trm)@x[nms],
                     descendants(trm)@x[nms])
    
})

test_that("show methods", {
    expect_null(show(trms))
    expect_null(show(trms[1]))
    expect_null(show(trms[1:2]))
    expect_null(show(trms[1:3]))
    expect_null(show(trms[1:4]))
    expect_null(show(trms[1:5]))
    expect_null(show(trm))
})

test_that("accessors", {
    expect_identical(length(olsSynonym(trms[1:2])), 2L)
    expect_false(isObsolete(trm))
    expect_true(isObsolete(term("GO", "GO:0005563")))
    expect_false(isObsolete(term("GO", "GO:0030533")))
    expect_equal(sum(isObsolete(trms)), 202)
    expect_equal(sum(!isObsolete(trms)), length(trms) - 202)

    expect_true(isRoot(trms[["SO:0000400"]]))
    expect_true(isRoot(trms[["SO:0000110"]]))
    expect_true(isRoot(trms[["SO:0002072"]]))
    expect_true(isRoot(trms[["SO:0001260"]]))

    expect_true(all(termId(olsRoot("SO")) %in% names(which(isRoot(trms)))))

    olsroot <- olsRoot("GO")
    goroots <- sort(structure(c("biological_process",
                           "cellular_component",
                           "molecular_function"),
                         .Names = c("GO:0008150",
                                    "GO:0005575", "GO:0003674")))
    expect_identical(sort(olsLabel(olsroot)), goroots)

    expect_identical(length(olsDesc(trms)), length(trms))
    expect_identical(length(olsLabel(trms)), length(trms))

    expect_true(all(unlist(olsNamespace(trms)) == "sequence"))
    expect_identical(olsNamespace(trm), "biological_process")
})

test_that("apply over Terms", {
    expect_identical(lapply(trms, termId),
                     termId(trms))
})

test_that("Term/Terms equality", {
    expect_true(all.equal(trms, trms))
    expect_true(all.equal(trms[1:10], trms[10:1]))
    expect_match(all.equal(trms[1:10], trms[1:2]),
                 "2 Terms are of different lengths")
    expect_match(all.equal(trms[1:10], trms[11:2]),
                 "Term ids don't match")
    i <- sample(length(trms), 10)
    for (ii in i) {
            cat("Testing term", termId(trms)[[ii]], "\n")
            all.equal(trms[[ii]], term("SO", termId(trms)[[ii]]))
    }
    expect_false(isTRUE(all.equal(trms[[1]], trms[[2]])))

    xx1 <- xx2 <- trms[1:2]
    expect_false(isTRUE(all.equal(xx1, xx2)))
    expect_match(all.equal(xx1, xx2), "Term id 'SO:0000579'")    
})
