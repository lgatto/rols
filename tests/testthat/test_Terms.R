context("Term/Terms")

go <- Ontology("GO")
trm <- term(go, "GO:0032801")
trms <- terms("SO", pagesize = 1000)

test_that("constructors", {
    so <- Ontology("SO")
    trms <- terms("SO")
    trms2 <- terms(so)
    expect_true(rols:::all.equal(trms, trms2))
    expect_identical(length(trms[1:10]), 10L)

    go <- Ontology("GO")
    trm <- term(go, "GO:0032801")
    trm1 <- term(go, "GO:0032801")
    trm2 <- term("go", "GO:0032801")
    trm3 <- term("GO", "GO:0032801")
    expect_identical(trm1, trm2)
    expect_identical(trm1, trm3)

    expect_identical(termPrefix(trm), "GO")
    expect_identical(termLabel(trm), "receptor catabolic process")
    expect_identical(termId(trm), "GO:0032801")


    trm1 <- trms[["SO:1000005"]]
    trm2 <- term("SO", "SO:1000005")
    expect_identical(trm1, trm2)
    expect_identical(termPrefix(trm1), "SO")
    expect_true(all(termPrefix(trms) == "SO"))

    expect_identical(termPrefix(trm), "GO")
    
    expect_identical(termSynonym(trm),
                     c("receptor breakdown",
                       "receptor degradation",
                       "receptor catabolism"))

    expect_identical(termDesc(trm),
                     "The chemical reactions and pathways resulting in the breakdown of a receptor molecule, a macromolecule that undergoes combination with a hormone, neurotransmitter, drug or intracellular messenger to initiate a change in cell function.")

    expect_identical(termLabel(trm), "receptor catabolic process")

    expect_identical(sort(termLabel(children(trm))),
                     sort(c('GO:0038018' = "Wnt receptor catabolic process",
                            'GO:1990172' = "G-protein coupled receptor catabolic process",
                            'GO:0032802' = "low-density lipoprotein particle receptor catabolic process",
                            'GO:0097019' = "neurotransmitter receptor catabolic process")))


    expect_identical(sort(termLabel(parents(trm))),
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
    expect_identical(length(termSynonym(trms[1:2])), 2L)
    expect_false(isObsolete(trm))
    expect_true(isObsolete(term("GO", "GO:0005563")))
    expect_false(isObsolete(term("GO", "GO:0030533")))
    k <- 204
    expect_equal(sum(isObsolete(trms)), k)
    expect_equal(sum(!isObsolete(trms)), length(trms) - k)

    expect_true(isRoot(trms[["SO:0000400"]]))
    expect_true(isRoot(trms[["SO:0000110"]]))
    expect_true(isRoot(trms[["SO:0001260"]]))

    expect_true(all(termId(olsRoot("SO")) %in% names(which(isRoot(trms)))))

    olsroot <- olsRoot("GO")
    goroots <- sort(structure(c("biological_process",
                           "cellular_component",
                           "molecular_function"),
                         .Names = c("GO:0008150",
                                    "GO:0005575", "GO:0003674")))
    expect_identical(sort(termLabel(olsroot)), goroots)

    expect_identical(length(termDesc(trms)), length(trms))
    expect_identical(length(termLabel(trms)), length(trms))

    expect_true(all(unlist(termNamespace(trms)) == "sequence"))
    expect_identical(termNamespace(trm), "biological_process")

    expect_true(all(unlist(termOntology(trms)) == "so"))
    expect_identical(termOntology(trm), "go")
})

test_that("unique terms", {
    x <- list(term("go", "GO:0005802"),
              term("nbo", "GO:0005802"),
              term("cco", "GO:0005802"))
    names(x) <- rep("GO:0005802", 3)
    trms <- rols:::Terms(x = x)
    expect_identical(length(trms), 3L)
    expect_identical(length(unique(trms)), 1L)
    expect_identical(unique(trms)[[1]], trms[[1]])
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
    xx1@x[[1]] <- xx1@x[[2]]
    expect_false(isTRUE(all.equal(xx1, xx2)))
    expect_match(all.equal(xx1, xx2), "Term id 'SO:0000579'")    
})

test_that("terms(pagesize)", {
    trms1 <- terms("SO", pagesize = 20)
    trms2 <- terms("SO", pagesize = 200)
    trms3 <- terms("SO", pagesize = 1000)
    trms4 <- terms("SO", pagesize = 10000) ## 2302 entries
    trms5 <- terms("SO", pagesize = 2302) ## 2302 entries
    expect_true(all.equal(trms1, trms2))
    expect_true(all.equal(trms1, trms3))
    expect_true(all.equal(trms3, trms4))
    expect_true(all.equal(trms3, trms5))
})

test_that("No links", {
    trm <- term("GO", "GO:0030232")
    ## does not have any children
    expect_message(x <- children(trm), "No children terms.")
    expect_null(x)
    ## does not have any descendants
    expect_message(x <- descendants(trm), "No descendant terms.")
    expect_null(x)
    ## does have parents and ancestors, though
    expect_is(parents(trm), "Terms")
    expect_is(ancestors(trm), "Terms")
    ## not anymore
    trm@links$parents <- NULL
    trm@links$ancestors <- NULL
    expect_message(x <- parents(trm), "No parent terms.")
    expect_null(x)
    expect_message(x <- ancestors(trm), "No ancestor terms.")
    expect_null(x)
})

test_that("partOf and derivesFrom", {
    pof <- partOf(term("GO", "GO:0008308"))
    expect_identical(length(pof), 1L)
    expect_identical(termId(pof[[1]]), "GO:1903959")

    pof <- partOf(term("BTO", "BTO:0000142"))
    expect_identical(length(pof), 2L)
    expect_identical(lapply(pof, termLabel),
                     list(`BTO:0000227` = "central nervous system",
                          `BTO:0000282` = "head"))

    defrom <- derivesFrom(term("BTO", "BTO:0002600"))
    expect_identical(length(defrom), 1L)
    expect_identical(termId(defrom[[1]]), "BTO:0000099")

    defrom <- derivesFrom(term("BTO", "BTO:0001023"))
    expect_identical(length(defrom), 1L)
    expect_identical(termId(defrom[[1]]), "BTO:0000975")

    expect_null(derivesFrom(term("GO", "GO:0008308")))
    expect_message(derivesFrom(term("GO", "GO:0008308")),
                   "No 'derives from' terms")

    expect_null(partOf(term("BTO", "BTO:0002600")))
    expect_message(partOf(term("BTO", "BTO:0002600")),
                   "No 'part of' terms")    
})
