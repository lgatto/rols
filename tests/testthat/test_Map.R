test_that("Map class", {
              map <- allIds("MS", simplify=FALSE)
              char <- allIds("MS", simplify=TRUE)              
              char2 <- as(map, "character")
              char3 <- as.character(map)
              expect_identical(char, char2)
              expect_identical(char, char3)
              k <- key(map)
              names(k) <- NULL
              expect_equal(k, names(char))
              v <- value(map)
              names(v) <- names(char)
              expect_equal(v, char)
              ## show -----------------------
              expect_true(is.null(show(map))) 
              expect_true(is.null(show(new("Map"))))
              expect_true(is.null(show(new("Map", .Data = map[1]))))
              ## ----------------------------
              xomapi <- map[[1]]
              x <- as(map[[1]], "character")
              y <- as.character(map[[1]])
              expect_identical(x, y)
          })

