\name{OlsSearch-class}
\Rdversion{1.1}
\docType{class}

\alias{OlsSearch}
\alias{class:OlsSearch}
\alias{OlsSearch-class}

\alias{olsSearch}

\alias{olsRows}
\alias{olsRows<-}
\alias{allRows}

\alias{coerce,OlsSearch,data.frame-method}
\alias{coerce,OlsSearch,Terms-method}

\alias{show,OlsSearch-method}

\title{Class \code{"OlsSearch"}}


\description{

  Searching the OLS is done using the \code{OlsSearch} data
  structure. 
  
}

\section{Objects from the Class}{
  
  Objects can be created with the constructor function
  \code{OlsSearch}. 

}
\section{Slots}{
  \describe{
    
    \item{\code{q}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{ontology}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{slim}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{fieldList}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{queryFields}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{exact}:}{Object of class \code{"logical"} ~~ }
    
    \item{\code{groupField}:}{Object of class \code{"logical"} ~~ }
    
    \item{\code{obsoletes}:}{Object of class \code{"logical"} ~~ }
    
    \item{\code{local}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{childrenOf}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{rows}:}{Object of class \code{"integer"} ~~ }
    
    \item{\code{start}:}{Object of class \code{"integer"} ~~ }
    
    \item{\code{url}:}{Object of class \code{"character"} ~~ }
    
    \item{\code{numFound}:}{Object of class \code{"integer"} ~~ }
    
    \item{\code{response}:}{Object of class \code{"data.frame"} ~~ }
    
  }
}

\section{Methods and functions}{
  \describe{
    
    \item{coerce}{\code{signature(from = "OlsSearch", to =
	"data.frame")}: ... }
    
    \item{coerce}{\code{signature(from = "OlsSearch", to = "Terms")}:
      ... }
    
    \item{show}{\code{signature(object = "OlsSearch")}: ... }


    \item{olsRows}{\code{signature(object = "OlsSearch")}: ... The value
      can be updated with the \code{olsRows} replacement method. To
      request all responses, use \code{allRows}. }

    
  }
}


\author{
Laurent Gatto <lg390@cam.ac.uk>
}


\examples{

OlsSearch(q = "trans-golgi")
OlsSearch(q = "cell")
OlsSearch(q = "cell", exact = TRUE)
OlsSearch(q = "cell", exact = TRUE, ontology = "go")
OlsSearch(q = "cell", exact = TRUE, ontology = "GO")

OlsSearch(q = "electrospray", ontology = "MS")
OlsSearch(q = "ionization", ontology = "MS")
OlsSearch(q = "electrospray ionization", ontology = "MS")
OlsSearch(q = "electrospray ionization", ontology = "MS", exact=TRUE)

## Request 5 results instead of 20 (default)
OlsSearch(q = "plasma,membrane", ontology = "go", rows = 5)

## or, once the object was created
(res <- OlsSearch(q = "plasma,membrane", ontology = "go"))
olsRows(res) <- 5
res
## all results
res <- allRows(res)
res

res <- OlsSearch(q = "trans-golgi", ontology = "go", rows = 5)
res
res <- olsSearch(res)
res
as(res, "data.frame")
res <- as(res, "Terms")
res
termPrefix(res)
termId(res)
}

\keyword{classes}
