context("queries and iface.R code")

## children/getTermChildren - not implemented
## getChildrenFromRoot - not implemented
## getTermsByExactName - not implemented
## getTermsByAnnotationData - not implemented

test_that("olsVersion/getVersion", {
              v <- olsVersion()
              expect_is(v, "character")
              expect_identical(length(v), 5L)
          })

test_that("term/getTermById", {
              expect_identical(term("GO:0005794", "GO"),
                               "Golgi apparatus")
              expect_null(term("GO:0000000", "GO"))
              expect_warning(term("GO:0000000", "GO"))
              expect_identical(term("210797", "NEWT"),
                               "Golfingia vulgaris (Marine worm)")
          })

test_that("termMetadata/getTermMetadata", {
              mtd <- termMetadata("GO:0005802", "GO")
              expect_null(rols:::print.termMetadata(mtd))
              expect_identical(mtd["exact_synonym_2"], c(exact_synonym_2 = "TGN"))
              expect_identical(length(mtd), 9L)
          })

test_that("termXrefs/getTermXrefs", {
              ans <- termXrefs("TGN transport vesicle", "GO")
              ans2 <- termXrefs("TGN", "GO")
              expect_equal(grep("GOC", ans), 1L)
              expect_equal(grep("GOC", ans2), 1L)
          })

test_that("onologyNames/getOntologyNames", {
              onms <- ontologyNames()
              expect_is(onms, "character")
              expect_true(length(onms) >= 93)
          })

test_that("ontologyLoadDate/getOntologyLoadDate", {
              onms <- ontologyNames()
              xx <- sapply(onms, ontologyLoadDate)
              expect_is(xx, "character")
              expect_identical(length(onms), length(xx))
          })

test_that("allIds/getAllTermsFromOntology", {
              allms <- allIds("MS", simplify=FALSE)
              expect_is(allms, "Map")
              expect_true(length(allms) >= 5266)
              allms2 <- allIds("MS", simplify=TRUE)
              expect_is(allms2, "character")
              expect_identical(length(allms), length(allms2))
          })

test_that("rootId/getRootTerms",{
              goroot <- rootId("GO")
              goans <- c('GO:0008150' = "biological_process",
                         'GO:0003674' = "molecular_function",
                         'GO:0005575' = "cellular_component")
              expect_identical(goroot, goans)              
          })

test_that("olsQuery/getTermsByName", {
              tgn <- olsQuery("tgn","GO")               
              tgnres <- c("TGN", "clathrin coat of TGN vesicle",
                          "TGN transport vesicle",
                          "TGN to endosome transport",
                          "TGN transport vesicle membrane")
              names(tgnres) <- c("GO:0005802",
                                 "GO:0030130",
                                 "GO:0030140",
                                 "GO:0006895",
                                 "GO:0012510")
              expect_identical(tgn, tgnres)

              esi2 <- olsQuery("ESI", "MS")
              esi1 <- olsQuery("ESI", "MS", exact = TRUE)
              expect_identical(length(esi1), 1L)
              expect_identical(length(esi2), 2L)
              expect_true(esi1 %in% esi2)

              expect_warning(olsQuery("tgn", exact=TRUE))
              x <- olsQuery("foobar", "GO", exact=TRUE)
              expect_equal(length(x), 0L)              
          })

test_that("olsQuery/getPrefixedTermsByName", {
              tgngo <- olsQuery("tgn","GO")
              tgn <- olsQuery("tgn") 
              expect_true(all(paste("GO", tgngo, sep = ":") %in% tgn))              
})

test_that("parents/getTermParents", {
              x <- parents("GO:0005802", "GO")
              ans <- structure(c("Golgi apparatus part", "Golgi apparatus",
                                 "Golgi subcompartment", "organelle subcompartment"),
                               .Names = c("GO:0044431", "GO:0005794",
                                   "GO:0098791", "GO:0031984"))
              expect_equal(x, ans)
          })


test_that("childrenRelations/getTermRelations", {
              x <- childrenRelations("GO:0005802", "GO")
              ans <- structure("part_of", .Names = "GO:0032588")
              expect_equal(x, ans)
              x <- childrenRelations("GO:0005802", "MS")
              ans <- structure(list(), .Names = character(0))
              expect_equal(x, ans)
})


test_that("isIdObsolete/isObsolete", {
              expect_true(isIdObsolete("GO:0005563", "GO"))
              expect_true(!isIdObsolete("GO:0030533", "GO"))
          })
