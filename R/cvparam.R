## CVParam <- function() { } ## constructor TODO

## a param is [CV label, accession, name, value]
setClass("CVParam",
         representation = representation(
           label = "character",
           accession = "character",
           name = "character",
           value = "character",
           user = "logical"),
         contains = "Versioned",
         prototype = prototype(
           user = FALSE,
           new("Versioned", versions=c(CVParam="0.2.0"))),
         validity = function(object) {
           msg <- validMsg(NULL, NULL)
           if (object@user) {
             if (!all(c(object@label, object@accession) == ""))
               msg <- "Label and accession must be empty in UserParams."
           } else {
             x <- c(object@label, object@accession,
                    object@name, object@value) == ""
             if (!all(x)) {
               .term <- term(object@accession, object@label)
               .synonyms <- termMetadata(object@accession, object@label)
               .synonyms <- .synonyms[grep("synonym", names(.synonyms))]
               if (!(object@name %in% c(.term, .synonyms)))
                 msg <- paste0("CVParam accession and name/synomyms do not match. Got [",
                               paste(c(.term, .synonyms), collapse = ", "),
                               "], expected '", object@name, "'.")
             }
           }
           if (is.null(msg)) TRUE else msg
         })                     

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
