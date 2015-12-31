\name{Ontology-class}
\Rdversion{1.1}
\docType{class}
\alias{Ontology-class}
\alias{olsDesc,Ontology-method}
\alias{olsPrefix,Ontology-method}
\alias{olsRoot,Ontology-method}
\alias{olsTitle,Ontology-method}
\alias{show,Ontology-method}
\alias{term,Ontology,character-method}
\alias{terms,Ontology-method}

\title{Class \code{"Ontology"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Ontology", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{loaded}:}{Object of class \code{"NullOrChar"} ~~ }
    \item{\code{updated}:}{Object of class \code{"NullOrChar"} ~~ }
    \item{\code{status}:}{Object of class \code{"NullOrChar"} ~~ }
    \item{\code{message}:}{Object of class \code{"NullOrChar"} ~~ }
    \item{\code{version}:}{Object of class \code{"NullOrChar"} ~~ }
    \item{\code{numberOfTerms}:}{Object of class \code{"integer"} ~~ }
    \item{\code{numberOfProperties}:}{Object of class \code{"integer"} ~~ }
    \item{\code{numberOfIndividuals}:}{Object of class \code{"integer"} ~~ }
    \item{\code{config}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{olsDesc}{\code{signature(object = "Ontology")}: ... }
    \item{olsPrefix}{\code{signature(object = "Ontology")}: ... }
    \item{olsRoot}{\code{signature(object = "Ontology")}: ... }
    \item{olsTitle}{\code{signature(object = "Ontology")}: ... }
    \item{show}{\code{signature(object = "Ontology")}: ... }
    \item{term}{\code{signature(object = "Ontology", id = "character")}: ... }
    \item{terms}{\code{signature(x = "Ontology")}: ... }
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
showClass("Ontology")
}
\keyword{classes}
