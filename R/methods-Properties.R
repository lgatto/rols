##########################################
## Constructors
setMethod("properties", "character",
          function(object, ...) .properties(object, ...))
setMethod("properties", "Ontology",
          function(object, ...) .properties(olsNamespace(object), ...))


## TODo - see https://github.com/EBISPOT/OLS/issues/36
## setMethod("properties", "Term", function(object, ...) ...)
## setMethod("properties", "Terms", function(object, ...) ...)

##########################################
## show methods

setMethod("show", "Property",
          function(object) {
              ids <- termId(object)
              cat("A Property from the", termPrefix(object), "ontology:", ids, "\n")
              cat(" Label: ", termLabel(object),"\n", sep = "")
          })


setMethod("show", "Properties",
          function(object) {
              cat("Object of class 'Properties' with", length(object), "entries\n")
              onts <- unique(termPrefix(object))
              if (length(onts) == 1)
                  cat(" From the", onts, "ontology\n")
              else if (length(onts) < 6)
                  cat(" From the", paste(onts, collapse = ", "), "ontologies\n")
              else cat(" From ", length(onts), "ontologies\n")
              n <- length(object)
              if (n > 4)
                  cat(" ", paste(head(termLabel(object), n=2), collapse = ", "),
                      "...",
                      paste(tail(termLabel(object), n=2), collapse = ", "), "\n")
              else
                  cat(paste(termLabel(object)[1:n], collapse = ", "), "\n")
          })
