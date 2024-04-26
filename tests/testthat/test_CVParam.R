test_that("User param creation", {
    up <- CVParam(name = "Hello", value = "World")
    expect_true(is.null(show(up)))
    expect_true(validObject(up))
    expect_error(CVParam(label = "labelonly"))
    expect_is(as("[GO, GO:0030330, DNA damage response, signal transduction by p53 class mediator, ]",
                 "CVParam"), "CVParam")
    ## works, but with a warning
    expect_warning(as("[GO, GO:0030330, dna damage response, signal transduction by p53 class mediator, ]",
                      "CVParam"))
})


test_that("User param to/from char", {
    up <- CVParam(name = "Hello", value = "World")
    upchar <- as(up, "character")
    expect_true(is.character(upchar)) ## works
    up2 <- as(upchar, "CVParam")
    expect_true(validObject(up2))
    expect_identical(up, up2)
})

test_that("CVParam creation and coercion", {
    cv <- CVParam(label = "GO", accession = "GO:0030330")
    cvchar <- as(cv, "character")
    cv2 <- as(cvchar, "CVParam")
    cv3 <- rols:::as.character.CVParam(cvchar)
    expect_identical(cv, cv2)
    expect_identical(cv2, cv2)
})

test_that("testing chars are CV params", {
    falses <- rols:::notvalidCVchars
    expect_false(any(charIsCVParam(falses)))
    falses <- gsub(" ", "", falses)
    expect_false(any(charIsCVParam(falses)))
    trues <- rols:::validCVchars
    expect_true(all(charIsCVParam(trues)))
})

test_that("Convert char to CVParam", {
    x <- "[GO, GO:0030330, DNA damage response, signal transduction by p53 class mediator, ]"
    cv <- as(x, "CVParam")
    expect_equal(cv@label, "GO")
    expect_equal(cv@accession, "GO:0030330")
    expect_equal(cv@name, "DNA damage response, signal transduction by p53 class mediator")
})

test_that("rep for CVParams", {
    up <- CVParam(name = "Hello", value = "World")
    n <- sample(1000, 1)
    rup <- rep(up, n)
    expect_equal(length(rup), n)
    rup <- rep(up, 3)
    expect_identical(rup[[1]], rup[[2]])
    expect_identical(rup[[1]], rup[[3]])
})
