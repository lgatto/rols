ol <- Ontologies()
go <- go1 <- Ontology("go")

test_that("Ontology constructors", {
    ## expect_equal(length(ol), 143L) ## this will likely change
    expect_true(length(ol) > 120L)
    expect_true(is.integer(length(ol)))
    expect_equal(length(ol[1:10]), 10L)
    ## Construction
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
    loaded <- olsLoaded(ol)
    ## --- Dates ---
    ## if the loaded date is not valid (NA), then that ontology should
    ## not have a status 'LOADED'.
    expect_true(all(which(is.na(loaded)) %in% which(status != "LOADED")))
    ## all update dates must be correct
    updated <- lubridate::ymd(olsUpdated(ol))
    expect_false(any(is.na(updated)))
    expect_identical(n, length(loaded))
    expect_identical(n, length(updated))
    expect_identical(olsLoaded(go), olsLoaded("GO"))
    expect_identical(olsLoaded(go), olsLoaded("go"))
    expect_identical(olsUpdated(go), olsUpdated("GO"))
    expect_identical(olsUpdated(go), olsUpdated("go"))
    ## --- Versions ---
    vrs <- olsVersion(ol)
    pre <- olsPrefix(ol)
    expect_identical(n, length(vrs))
    expect_identical(n, length(pre))
    ## expect_identical(vrs[["go"]], olsVersion(go))
    expect_identical(olsVersion("GO"), olsVersion(go))
    expect_identical(olsVersion("go"), olsVersion(go))
    ## --- Prefix ---
    i <- which(olsPrefix(ol) == "GO")
    expect_identical(pre[[i]], olsPrefix(go))
    expect_identical(pre[[i]], olsPrefix("go"))
    expect_identical(pre[[i]], olsPrefix("GO"))
    expect_identical(pre[[i]], olsPrefix("Go"))
    ## --- Description ---
    desc <- olsDesc(ol[[i]])
    expect_identical(desc, olsDesc(go))
    expect_identical(desc, olsDesc("go"))
    expect_identical(desc, olsDesc("GO"))
    ## --- Status ---
    expect_identical(status[[i]], olsStatus(go))
    expect_identical(status[[i]], olsStatus("go"))
    expect_identical(status[[i]], olsStatus("GO"))
    ## --- Namespace ---
    nsp0 <- olsNamespace(ol)
    nsp <- sapply(ol@x, olsNamespace)
    expect_identical(nsp0, nsp)
    expect_identical(nsp[[i]], olsNamespace("GO"))
    expect_identical(nsp[[i]], olsNamespace("go"))
    expect_identical(nsp[[i]], olsNamespace(go))
})

test_that("apply over Ontologies", {
    expect_identical(lapply(ol, olsPrefix),
                     olsPrefix(ol))
})

test_that("coercion", {
    odf <- as(ol, "data.frame")
    expect_equal(nrow(odf), length(ol))
    expect_equal(names(odf), c("Prefix", "Namespace", "Title"))
    olst <- as(ol, "list")
    expect_identical(olst, ol@x)
})
