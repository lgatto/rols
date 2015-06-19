context("CVParams creation and coercion")

test_that("CVParam creation and coercion", {
              cv <- CVParam(label = "MS", accession = "MS:1000073")
              cvchar <- as(cv, "character")
              cv2 <- as(cvchar, "CVParam")
              expect_equal(cv, cv2)
          })


test_that("CVParam creation and coercion", {
              cv <- CVParam(label = "MS",
                            accession = "MS:1000073")
              cv2 <- CVParam(label = "MS",
                            name = cv@name)
              expect_equal(cv, cv2)
          })

test_that("testing chars are CV params", {
              falses <- rols:::notvalidCVchars
              expect_false(any(charIsCVParam(falses)))
              falses <- gsub(" ", "", falses)
              expect_false(any(charIsCVParam(falses)))
              trues <- rols:::validCVchars
              expect_true(all(charIsCVParam(trues)))
          })
