context("Term/Terms")

test_that("constructors", {
    go <- Ontology("GO")    
    trm <- trm1 <- term(go, "GO:0032801")
    trm2 <- term("go", "GO:0032801")
    trm3 <- term("GO", "GO:0032801")
    expect_identical(trm1, trm2)
    expect_identical(trm1, trm3)

    expect_identical(olsPrefix(trm), "GO")
    expect_identical(olsLabel(trm), "receptor catabolic process")
    expect_identical(termId(trm), "GO:0032801")
    

})
