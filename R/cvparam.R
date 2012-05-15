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
