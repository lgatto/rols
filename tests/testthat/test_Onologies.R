context("Ontology/Ontologies")

ol <- Ontologies()
go <- go1 <- Ontology("go")

test_that("Ontology constructors", {

    ol1 <- Ontologies(50)
    expect_true(all.equal(ol, ol1))
    
    ## expect_equal(length(ol), 143L) ## this will likely change
    expect_true(length(ol) > 120L)
    expect_true(is.integer(length(ol)))
    expect_equal(length(ol[1:10]), 10L)
    
    go1 <- Ontology("go")
    go2 <- Ontology("GO")
    go3 <- Ontology("Go")
    expect_true(all.equal(go1, go))
    expect_true(all.equal(go1, go2))
    expect_true(all.equal(go1, go3))
    expect_true(all.equal(go1, ol[["go"]]))
})

test_that("Ontology show methods", {
    expect_null(show(ol))
    expect_null(show(ol[1]))
    expect_null(show(ol[1:2]))
    expect_null(show(ol[1:3]))
    expect_null(show(ol[1:4]))
    expect_null(show(ol[1:5]))
    expect_null(show(go1))
})

test_that("Ontology accessors", {
    library("lubridate")
    n <- length(ol)
    status <- olsStatus(ol)
    ## if the loaded date is not valid (NA), then that ontology should
    ## not have a status 'LOADED'.    
    expect_warning(loaded <- lubridate::ymd(olsLoaded(ol))) 
    ## expect_true(all(which(is.na(loaded)) %in% which(status != "LOADED")))
    ## all update dates must be correct
    updated <- lubridate::ymd(olsUpdated(ol))
    expect_false(any(is.na(updated)))
    expect_identical(n, length(loaded))
    expect_identical(n, length(updated))
    i <- which(names(status) == "go")
    expect_identical(ymd(olsLoaded(go)), loaded[i])
    expect_identical(olsLoaded(go), olsLoaded("GO"))
    expect_identical(olsLoaded(go), olsLoaded("go"))
    expect_identical(olsUpdated(go), olsUpdated("GO"))
    expect_identical(olsUpdated(go), olsUpdated("go"))

    vrs <- olsVersion(ol)
    pre <- olsPrefix(ol)
    expect_identical(n, length(vrs))
    expect_identical(n, length(pre))
    expect_identical(vrs[["go"]], olsVersion(go))
    expect_identical(olsVersion("GO"), olsVersion(go))
    expect_identical(olsVersion("go"), olsVersion(go))

    rts <- olsRoot(ol["go"])
    gort <- rts[[1]]
    expect_identical(gort, olsRoot(go))
    expect_identical(gort, olsRoot("go"))
    expect_identical(gort, olsRoot("GO"))
    trms <- rols:::Terms(x = list('GO:0005575' = term("GO", 'GO:0005575'),
                                  'GO:0003674' = term("GO", 'GO:0003674'),
                                  'GO:0008150' = term("GO", 'GO:0008150')))
    trms <- trms[order(termId(trms))]
    gort <- gort[order(termId(gort))]
    expect_identical(trms, gort)
    expect_identical(pre[[i]], olsPrefix(go))
    expect_identical(pre[[i]], olsPrefix("go"))
    expect_identical(pre[[i]], olsPrefix("GO"))
    expect_identical(pre[[i]], olsPrefix("Go"))
   
    desc <- olsDesc(ol)
    expect_identical(desc[[i]], olsDesc(go))
    expect_identical(desc[[i]], olsDesc("go"))
    expect_identical(desc[[i]], olsDesc("GO"))

    ttl <- olsTitle(ol)
    expect_identical(ttl[[i]], olsTitle(go))
    expect_identical(ttl[[i]], olsTitle("go"))
    expect_identical(ttl[[i]], olsTitle("GO"))

    expect_identical(olsTitle(go), "Gene Ontology")
    expect_identical(olsDesc(go), "An ontology for describing the function of genes and gene products")

    ## expect_identical(status[[i]], "LOADED") ## failed Sun Jan  1 20:36:00 GMT 2017
    expect_identical(status[[i]], olsStatus(go))
    expect_identical(status[[i]], olsStatus("go"))
    expect_identical(status[[i]], olsStatus("GO"))

    nsp0 <- olsNamespace(ol)
    nsp <- sapply(ol@x, olsNamespace)
    expect_identical(nsp0, nsp)
    expect_identical(nsp[["go"]], olsNamespace("GO"))
    expect_identical(nsp[["go"]], olsNamespace("go"))
    expect_identical(nsp[["go"]], olsNamespace(go))
})

test_that("apply over Ontologies", {
    expect_identical(unlist(lapply(ol, olsPrefix)),
                     olsPrefix(ol))
})

test_that("coercion", {
    odf <- as(ol, "data.frame")
    expect_equal(nrow(odf), length(ol))
    expect_equal(names(odf), c("Prefix", "Namespace", "Title"))

    olst <- as(ol, "list")
    expect_identical(olst, ol@x)
})

test_that("all.equal ontolgies", {
    ol0 <- ol <- Ontologies()
    expect_identical(length(ol), length(ol[-1]) + 1L)
    expect_identical(all.equal(ol, ol[-1]),
                     "The 2 Ontologies are of different lengths")
    names(ol@x)[1] <- "foo"
    expect_identical(length(ol), length(ol0))
    expect_identical(all.equal(ol, ol0),
                     "Ontology names don't match")
    expect_identical(names(ol@x)[-1], names(ol0@x)[-1])
    ol <- ol0
    ol@x[[1]]@loaded <- "123"
    nm <- olsNamespace(ol[[1]])
    expect_equal(all.equal(ol, ol0),
                 paste0("Ontology '", nm, "': loaded: 1 string mismatch"))
})
