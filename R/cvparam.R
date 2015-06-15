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
        stop("Found more than one matching term: ", paste(.term, collapse = ", "))
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
    err <- paste("Your input character should be",
                 "'[MS, MS:1000073, ESI, ]'.",
                 "See ?CVParam for details.")
    ## trim leading and trailing whitespace
    trim <- function (x) gsub("^\\s+|\\s+$", "", x)
    from <- trim(from)
    ## First and last chars must be '[' and ']'
    valid <- c(substr(from, 1, 1) == "[",
               substr(from, nchar(from), nchar(from)) == "]")
    if (!all(valid))              
        stop(err)
    from <- substr(from, 2, nchar(from)-1)
    from <- strsplit(from, ",")[[1]]
    if (length(from) != 4) stop(err)
    ## Assuming correct order here!
    names(from) <- c("label", "accession", "name", "value")
    from <- sapply(from, trim)
    if (from["value"] != "") { ## User param
        cv <- CVParam(name = from["name"],
                      value = from["value"])
    } else { ## CV para
        cv <- CVParam(label = from["label"], 
                      accession = from["accession"])
        if (from["name"] != "" && cv@name != from["name"])
            warning("The CV names did not match:\n  ",
                    "Yours: '", from["name"], "' - OLS: '", cv@name, "'.")
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
