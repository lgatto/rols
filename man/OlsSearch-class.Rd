\name{OlsSearch-class}
\Rdversion{1.1}
\docType{class}
\alias{OlsSearch-class}
\alias{coerce,OlsSearch,data.frame-method}
\alias{coerce,OlsSearch,Terms-method}
\alias{show,OlsSearch-method}

\title{Class \code{"OlsSearch"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OlsSearch", ...)}.
%%  ~~ describe objects here ~~ 
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
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "OlsSearch", to = "data.frame")}: ... }
    \item{coerce}{\code{signature(from = "OlsSearch", to = "Terms")}: ... }
    \item{show}{\code{signature(object = "OlsSearch")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{

OlsSearch(q = "trans-golgi")
OlsSearch(q = "cell")
OlsSearch(q = "cell", exact = TRUE)
OlsSearch(q = "cell", exact = TRUE, ontology = "go")
OlsSearch(q = "cell", exact = TRUE, ontology = "GO")

OlsSearch(q = "plasma", ontology = "go")
OlsSearch(q = "membrane", ontology = "go")
OlsSearch(q = "plasma,membrane", ontology = "go")

res <- OlsSearch(q = "trans-golgi", ontology = "go", rows = 5)
res
res <- olsSearch(res)
res
as(res, "data.frame")
res <- as(res, "Terms")

olsPrefix(res)
termId(res)
}

\keyword{classes}
