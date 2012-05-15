## a param is [CV label, accession, name|synonym, value]
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
