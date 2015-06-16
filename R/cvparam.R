## trim leading and trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

CVParam <- function(label,
                    name,
                    accession,
                    value,
                    exact = TRUE) {
  if (missing(label)) {
    ## a User param
    ans <- new("CVParam", name = name, user = TRUE)
  } else {    
    ## a CV param
    if (missing(name) & missing(accession)) {
      stop("You need to provide at least one of 'name' or 'accession'")
    } else if (missing(name)) {
      name <- term(accession, label)
    } else { ## missing(accession)
      .term <- olsQuery(name, label, exact = exact)
      if (length(.term) != 1)
          stop("Found more than one matching term: ",
               paste(.term, collapse = ", "))
      accession <- names(.term)
    }
    
    ans <- new("CVParam", label = label, name = name, accession = accession)
  }
  if (!missing(value))
    ans@value <- value
  
  if (validObject(ans))
    return(ans)
} 

setAs("CVParam", "character",
      function(from, to = "character") {
        ans <- paste0("[",
                      from@label, ", ",
                      from@accession, ", ",
                      from@name, ", ",
                      from@value, "]")
        ans
      })
as.character.CVParam <- function(x, ...) as(x, "character")

setMethod("show","CVParam",
          function(object) {
            cat(as(object, "character"), "\n")
            invisible(NULL)
          })



setMethod("rep", "CVParam",
          function(x, times) {
            l <- vector("list", length = times)
            for (i in 1:times)
              l[[i]] <- x
            return(l)
          })

cvCharToCVPar <- function(from) {
    stopifnot(length(from) == 1)
    if (!charIsCVParam(from))
            stop(paste("Your input character should be",
                       "'[MS, MS:1000073, ESI, ]'.",
                       "See ?CVParam for details."))
    from <- substr(from, 2, nchar(from)-1)
    from <- strsplit(from, ",")[[1]]

    ## Assuming correct order here!
    ## 1: "label", 2: "accession", 3: "name", 4: "value"
    from <- sapply(from, trim, USE.NAMES = FALSE)
    if (from[1] != "") { ## label is missing -> user param
        cv <- CVParam(name = from[3], value = from[4])
    } else { ## CV para
        cv <- CVParam(label = from[1], accession = from[2])
        if (from[3] != "" && cv@name != from[3])
            warning("The CV names did not match:\n  ",
                    "Yours: '", from[3], "' - OLS: '", cv@name, "'.")
    }
    cv
}

setAs("character", "CVParam",
      function(from, to = "CVParam") {
          ans <- lapply(from, cvCharToCVPar)
          if (length(ans) == 1)
              ans <- ans[[1]]
          ans
      })

as.character.CVParam <- function(x, ...) as(x, "character")

charIsCVParam <- function(x) {
    stopifnot(is.character(x))
    x <- trim(x)
    rx <- "\\[*([A-Z]+)?, *([A-Z]+:[[:digit:]]+)?, *([[:print:]]+)?, *([[:print:]]+)?\\]"
    valid <- grepl(rx, x)
    x <- substr(x, 2, nchar(x)-1)
    x <- strsplit(x, ",")
    valid2 <-
        sapply(x, function(xx)
            ifelse(xx[1] %in% ontologies()[, 1], TRUE, FALSE))
    ## TODO: verify that either 3,4 for user param
    ##                       or 1,2 for cv param
    ##       are present
    return(valid & valid2)
}
