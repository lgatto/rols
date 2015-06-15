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
