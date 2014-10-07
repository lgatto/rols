##' This function returns the Ontology Lookup Webservice
##' version, build data and author. It sends a
##' \code{getVersionRequest} SOAP message and retrieves
##' and parses the \code{getVersionResponse}.
##' The original interface is \code{public String getVersion()}.
##' 
##' @title Returns the OLS version
##' @return A \code{character} of length 5.
##' @author Laurent Gatto
##' @export
##' @family ols-queries
##' @examples
##' olsVersion()
olsVersion <- function() {
  xx <- getVersion(.convert=TRUE)
  xx <- unlist(strsplit(xx, "\n"))
  xx <- unlist(strsplit(xx, "\\$"))
  xx <- xx[xx!=""]
  xx <- sub(" $", "", xx)
  return(xx)
}


##' This function returns available ontologies.
##' It sends a \code{getOntologyNamesRequest} SOAP message
##' and retrieves and parses the \code{getOntologyNamesResponse}.
##' The original interface is \code{public Map getOntologyNames()}.
##' 
##' @title Returns all available ontologies
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return If \code{simplify} is \code{TRUE}, a \code{data.frame}
##' with available ontologies names and descriptions. An instance
##' of class \code{Map} otherwise.
##' @author Laurent Gatto
##' @export
##' @family ols-queries
##' @examples
##' head(ontologies())
##' ontologies(simplify=FALSE)
ontologies <- function(simplify=TRUE) {
  ans <- getOntologiesFromEnv() ## 'Map' object
  if (is.null(ans)) {
    ## if not yet cached, retrive the ontologies
    ## from the OLS server and store them locally.
    xx <- getOntologyNames() ## 'getOntologyNamesReturn' object
    ans <- map(xx)
    unlockBinding("ontologies", .rolsEnv)
    assign("ontologies", ans, envir = .rolsEnv)
    lockBinding("ontologies", .rolsEnv)
  }
  if (simplify) {
    ans <- data.frame(t(sapply(ans, as, "character")),
                      row.names = NULL)
    colnames(ans) <- c("Name","Description")
    rownames(ans) <- ans$Name
  }
  return(ans)
}

##' Returns the names of the OLS ontologies.
##'
##' @title Returns all ontologyNames
##' @return A \code{charcter} with all ontology names.
##' @author Laurent Gatto
##' @export
##' @family ols-queries
##' @examples
##' head(ontologyNames())
ontologyNames <- function() {
  as.character(ontologies(simplify=TRUE)$Name)
}

##' This function returns the load date of a given
##' ontology. The ontology name must be valid, i.e. exists
##' in \code{ontologies()}.
##' It sends a \code{getOntologyLoadDateRequest} SOAP message
##' and retrieves and parses the \code{getOntologyLoadDateResponse}.
##' The original interface is
##' \code{public String getOntologyLoadDate(String ontologyName)}.
##' 
##' @title Returns the ontology load date
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @return A \code{character} with the ontology's load date. 
##' @author Laurent Gatto
##' @export
##' @family ols-queries
##' @examples
##' ontologyLoadDate("GO")
##' ontologyLoadDate("FIX")
ontologyLoadDate <- function(ontologyName) {
  if (missing(ontologyName))
    stop(paste("Specify which ontology you want the load date of.",
               "See ontolgyNames() for possible ontologies.",sep=""))
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getOntologyLoadDate(ontologyName=ontologyName,
                            .convert=TRUE)
  return(xx)
}

##' When terms are found to be outside the scope of an ontology,
##' are misleadingly named or defined or describe a concept that
##' would be better represented in another way, the terms are marked
##' obsolete rather than deleted. This function tests this by
##' sending an \code{isObsoleteRequest} SOAP message
##' and retrieves and parses the \code{isObsoleteResponse}.
##' The original interface is
##' \code{public boolean isObsolete(String termId, String ontologyName)}.
##' 
##' @title Is the ontology id obsolete
##' @param termId A \code{character} with a valid ontology identifier.
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @return A \code{logical} specifying if the term id is obsolete.
##' @author Laurent Gatto
##' @family ols-queries
##' @examples
##' ## is obsolete
##' term("GO:0005563", "GO")
##' isIdObsolete(termId = "GO:0005563", ontologyName = "GO")
##' stopifnot(isIdObsolete(termId = "GO:0005563", ontologyName = "GO"))
##' ## replaced by
##' term("GO:0030533", "GO")
##' isIdObsolete(termId = "GO:0030533", ontologyName = "GO") 
##' @export
isIdObsolete <- function(termId, ontologyName) {
  if (missing(ontologyName))
    stop(paste("Specify the ontology name to search ",termId," in.",
               "See ontolgyNames() for possible ontologies.",sep=""))
    ontologyName <- match.arg(ontologyName, ontologyNames())
  return(as.logical(isObsolete(termId = termId,
                               ontologyName=ontologyName,
                               .convert=TRUE)))
}

##' This function returns the term (description) of a given
##' ontology identifier in a specific ontology. The ontology name
##' must be valid, i.e. exists in \code{ontologies()}.
##' It sends a \code{getTermByIdRequest} SOAP message
##' and retrieves and parses the \code{getTermByIdResponse}.
##' The original interface is
##' \code{public String getTermById(String termId, String ontologyName)}.
##' 
##' @title Returns the term of a given identifier
##' @param termId A \code{character} with a valid ontology identifier.
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @return A \code{string} with the description of that identifier,
##' as found in ontolgy \code{ontologies}. If \code{termId} was not found
##' in \code{ontologies()}, as warning is issued and NULL is returned.
##' @author Laurent Gatto
##' @export
##' @family ols-queries
##' @examples
##' term("GO:0005794", "GO") ## valid description
##' term("GO:0000000", "GO") ## returns NULL
##' term("210797", "NEWT") 
term <- function(termId, ontologyName) {
  if (missing(ontologyName))
    stop(paste("Specify the ontology name to search ",termId," in.",
               "See ontolgyNames() for possible ontologies.",sep=""))
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getTermById(termId=termId, ontologyName=ontologyName, .convert=TRUE)
  if (xx == termId) {
    warning(paste("Idenifier ", termId, " not found in ",
                  "ontology ", ontologyName, sep=""))
    return(NULL)
  }
  return(xx)
}


##' This function returns the metadata (definition and synonyms)
##' for a specific ontology identifier. The term for that identifier
##' can be retrieved with \code{\link{term}}. 
##' The function sends a \code{getTermMetadataRequest} SOAP message
##' and retrieves and parses the \code{getTermMetadataResponse}.
##' The original interface is
##' \code{public Map getTermMetadata(String termId, String ontologyName)}.
##' 
##' @title Retuns an identifier's metadata
##' @param termId A \code{character} with a valid ontology identifier.
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return Am S3 instance of class \code{termMetadata} (for pretty
##' printing) and \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise.
##' @author Laurent Gatto
##' @family ols-queries
##' @export
##' @examples
##' termMetadata("GO:0005794", "GO")
##' termMetadata("210797", "NEWT")
termMetadata <- function(termId, ontologyName, simplify=TRUE) {
  if (missing(ontologyName))
    stop(paste("Specify the ontology name to search ",termId," in.",
               "See ontolgyNames() for possible ontologies.",sep=""))
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getTermMetadata(termId=termId,
                        ontologyName=ontologyName)
  ans <- map(xx)
  if (simplify) {
      ans <- as(ans, "character")
      class(ans) <- c("termMetadata", "character")
  }
  return(ans)
}

print.termMetadata = function(x, ...) {
     txt <- paste(names(x), x, sep=": ")
     cat(noquote(paste(strwrap(txt, exdent=2), collapse="\n")), "\n")
}

##' This function returns ontology cross references
##' for an identifier.  The function sends a
##' \code{getTermXrefsRequest} SOAP message
##' and retrieves and parses the \code{getTermXrefsResponse}.
##' The original interface is
##' \code{public Map getTermXrefs(String termId, String ontologyName)}.
##'
##' @title Returns the idenifier's ontology cross references
##' @param termId A \code{character} with a valid ontology identifier.
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return A named \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise. 
##' @author Laurent Gatto
##' @family ols-queries
##' @export
termXrefs <- function(termId, ontologyName, simplify=TRUE) {
 if (missing(ontologyName))
   stop(paste("Specify the ontology name to search ",termId," in.",
              "See ontolgyNames() for possible ontologies.",sep=""))
 ontologyName <- match.arg(ontologyName, ontologyNames())
 xx <- getTermXrefs(termId=termId,
                    ontologyName=ontologyName)
 ans <- map(xx)
 if (simplify) 
   ans <- as(ans, "character")
 return(ans)
}


##' This function returns root identifier(s) for a
##' given valid ontology name. It sends a
##' \code{getRootTermsRequest} SOAP message
##' and retrieves and parses the \code{getRootTermsResponse}.
##' The original interface is
##' \code{public Map getRootTerms(String ontologyName)}.
##'
##' @title Retuns the root identifiers of an ontology
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return A named \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise. 
##' @author Laurent Gatto
##' @family ols-queries
##' @export
##' @examples
##' rootId("GO")
##' rootId("NEWT")
##' rootId("MS")
rootId <- function(ontologyName, simplify=TRUE) {
  if (missing(ontologyName))
    stop("Specify an ontology name.")
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getRootTerms(ontologyName = ontologyName)
  ans <- map(xx)
  if (simplify) 
    ans <- as(ans, "character")
  return(ans)
}


##' This function returns all identifiers and terms available 
##' for given valid ontology. It sends a
##' \code{getAllTermsFromOntologyRequest} SOAP message
##' and retrieves and parses the \code{getAllTermsFromOntologyResponse}.
##' The original interface is
##' \code{public Map getAllTermsFromOntology(String ontologyName)}.
##'
##' @title Returns all identifiers and terms of an ontology
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return A named \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise. 
##' @family ols-queries
##' @export
##' @author Laurent Gatto
##' @examples
##' allIds("MS", simplify=FALSE)
allIds <- function(ontologyName, simplify=TRUE) {
  if (missing(ontologyName))
    stop("Specify an ontology name.")
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getAllTermsFromOntology(ontologyName = ontologyName)
  ans <- map(xx)
  if (simplify) 
    ans <- as(ans, "character")
  return(ans) 
}

##' This function queries one or all ontologies for a pattern
##' and returns all identifiers/terms. If a valid \code{ontologyName}
##' is provided, only that ontology is queried. The function then sends a
##' \code{getTermsByNameRequest} SOAP message
##' and retrieves and parses the \code{getTermsByNameResponse}.
##' The original corresponging interface is
##' \code{public Map getTermsByName(String partialName, String ontologyName, boolean reverseKeyOrder)}.
##' If no \code{ontologyName} is provided, all ontologies are used; the function then
##' sends a \code{getPrefixedTermsByNameRequest} SOAP message
##' and retrieves and parses the \code{getPrefixedTermsByNameResponse}.
##' The original corresponging interface is
##' \code{public Map getPrefixedTermsByName(String partialName, boolean reverseKeyOrder)}.
##'
##' Some valid queries sometimes return empty results due to network instabilities.
##' For this reason, each \code{olsQuery} is repeated 3 times (see \code{n} parameter)
##' as long as empty resuls are obtained. In general, when the ontology is specified,
##' queries are fast and reliable. 
##' 
##' @title Returns matching identifiers
##' @param pattern A \code{character} used to query the OLS.
##' @param ontologyName Optional. A \code{character} with the name of
##' a valid ontology name. If missing, all ontologies are searched for
##' \code{pattern}.
##' @param exact Require pattern to match term exactly. Default is
##' FALSE. Note that if \code{ontologyName} is missing, exact is
##' ignored.
##' @param n Number of attempts to repeat the query if no result is
##' found. Default is 3. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return A named \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise. 
##' @author Laurent Gatto
##' @family ols-queries
##' @export
##' @examples
##' olsQuery("tgn","GO") ## search GO for 'tgn'
##' olsQuery("tgn") ## search all ontologies
##' olsQuery("ESI", "MS")
##' olsQuery("ESI", "MS", exact = TRUE)
olsQuery <- function(pattern, ontologyName,
                     exact = FALSE, n = 3,
                     simplify = TRUE) {
  ans <- list()
  .n <- n
  while (length(ans) == 0 & n > 0) {
    if (missing(ontologyName)) {
      ans <- getPrefixedTermsByName(partialName = pattern,
                                   reverse = FALSE)
    } else {
      ontologyName <- match.arg(ontologyName, ontologyNames())
      ans <- getTermsByName(partialName = pattern,
                            ontologyName = ontologyName,
                            reverse = FALSE)
    }
    ans <- map(ans)
    n <- n - 1
  }
  if (length(ans) == 0) 
    message("Empty query results after ", .n, " attempts.")

  if (exact) {
    if (missing(ontologyName)) {
      warning("Ignoring 'exact' when 'ontologyName' is missing.")
    } else {
      i <- which(value(ans) == pattern)
      if (length(i) == 0) {
        ans <- new("Map") ## empty
      } else {
        ans <- new("Map", .Data = ans[i])
      }
    }
  }
  if (simplify) 
    ans <- as(ans, "character")
  return(ans) 
}


##' This function returns the parent term(s) of term \code{termId}
##' in ontology \code{ontologyName}. 
##' The function sends a \code{getTermParentsRequest} SOAP message
##' and retrieves and parses the \code{getTermParentsResponse}.
##' The original corresponging interface is
##' \code{public Map getTermParents(String termId, String ontologyName)}
##' 
##' @title Returns the parent(s) of a term.
##' @param termId A \code{character} with a valid ontology identifier.
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return A named \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise. 
##' @author Laurent Gatto
##' @export
##' @examples
##' parents("GO:0005802", "GO")
parents <- function(termId = termId,
                    ontologyName = ontologyName,
                    simplify=TRUE) {
  if (missing(ontologyName))
    stop("Specify an ontology name.")
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getTermParents(termId = termId,
                       ontologyName = ontologyName)
  ans <- map(xx)
  if (simplify) 
    ans <- as(ans, "character")
  return(ans) 
}

##' This function returns the relation type of a ontology term 
##' \code{termId} and its children.
##' The function sends a \code{getTermRelationsRequest} SOAP message
##' and retrieves and parses the \code{getTermRelationsResponse}.
##' The original corresponging interface is
##' \code{public Map getTermRelations(String termId, String ontologyName)}.
##'
##' @title Returns the children relation type(s).
##' @param termId A \code{character} with a valid ontology identifier.
##' @param ontologyName A \code{character} with the name of a valid ontology name. 
##' @param simplify A logical indicating whether the S4 \code{Map}
##' instance should be simplified. Default is \code{TRUE}.
##' @return A named \code{character} if \code{simplify} is \code{TRUE}.
##' An instance of class \code{Map} otherwise. 
##' @author Laurent Gatto
##' @export
##' @examples
##' childrenRelations("GO:0005802", "GO")
childrenRelations <- function(termId = termId,
                              ontologyName = ontologyName,
                              simplify=TRUE) {
  if (missing(ontologyName))
    stop("Specify an ontology name.")
  ontologyName <- match.arg(ontologyName, ontologyNames())
  xx <- getTermRelations(termId = termId,
                         ontologyName = ontologyName)
  ans <- map(xx)
  if (simplify) 
    ans <- as(ans, "character")
  return(ans) 
}


## ## public Map getTermChildren(String termId, String ontologyName, int distance, int[] relationTypes)
## ## distance: the cutoff distance. if distance < 0, will return all children
## ## raltionTypes: the types of relation between the terms ...
## children <- function(termId = termId,
##                      ontologyName = ontologyName,
##                      distance = 1,  ## NULL: all children or numeric to get tems <= distance 
##                      realtionTypes, ## NULL: all types, other? 1: is_a, 2:part_if, 3: both
##                      simplify=TRUE) {
##   if (missing(ontologyName))
##     stop("Specify an ontology name.")
##   ontologyName <- match.arg(ontologyName, ontologyNames())
##   if (missing(relationTypes)) {
##     relationTypes <- 3
##   } else {
##     relationTypes <- switch(relationTypes,
##                            "is_a" = 1,
##                            "part_of" = 2)
##     if (is.null(relationTypes))
##       stop("If specified, 'relationTypes' must be 'part_of' or 'is_a'.")           
##   }
##   xx <- getTermChildren(termId = termId,
##                         ontologyName = ontologyName,
##                         distance = distance,
##                         relationTypes = relationTypes)
##   ans <- map(xx)
##   if (simplify) 
##     ans <- as(ans, "character")
##   return(ans) 
## }


## public Map getChildrenFromRoot(String RootTermId, String ontologyName, Vector childrenIds);
## needs a set("character","Vector") method

## public DataHolder[] getTermsByAnnotationData(String ontologyName, String annotationType, String strValue, Double fromDblValue, Double toDblValue);

