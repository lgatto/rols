##########################################
## Constructors
setMethod("properties", "character",
          function(object, ...) .properties(object, ...))
setMethod("properties", "Ontology",
          function(object, ...) .properties(olsNamespace(object), ...))

setMethod("properties", "Term",
          function(object, ...) {
              urls <- getPropertyLinks(object)
              if (length(urls) == 0) {
                  message("No properties for term ", termId(object))
                  return(NULL)
              }
              ans <- lapply(urls, makeProperties)
              ans <- unlist(lapply(ans, "slot", "x"))
              names(ans) <- sub("\\.href\\.", "/", names(ans))
              Properties(x = ans)
          })

setMethod("properties", "Terms",
          function(object, ...) {
              lapply(object@x, properties)
          })

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
              cat("Object of class 'Properties' with", length(object),
                  ifelse(length(object) > 1,
                         "entries\n",
                         "entry\n"))
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

##########################################
## Data manipulation

setMethod("length", "Properties", function(x) length(x@x))
