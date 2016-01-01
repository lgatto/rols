\name{Term-class}
\Rdversion{1.1}
\docType{class}
\alias{Term-class}
\alias{Terms-class}

\alias{olsLabel,Term-method}
\alias{olsPrefix,Term-method}

\alias{show,Term-method}
\alias{show,Terms-method}

\alias{termId}
\alias{termId,Term-method}
\alias{termId,Terms-method}

\alias{term,Ontology,character-method}
\alias{terms,Ontology-method}

\title{Class \code{"Term"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Term", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{iri}:}{Object of class \code{"character"} ~~ }
    \item{\code{label}:}{Object of class \code{"character"} ~~ }
    \item{\code{description}:}{Object of class \code{"NullOrList"} ~~ }
    \item{\code{annotation}:}{Object of class \code{"list"} ~~ }
    \item{\code{synonym}:}{Object of class \code{"NullOrList"} ~~ }
    \item{\code{ontology_name}:}{Object of class \code{"character"} ~~ }
    \item{\code{ontology_prefix}:}{Object of class \code{"character"} ~~ }
    \item{\code{ontology_iri}:}{Object of class \code{"character"} ~~ }
    \item{\code{is_obsolete}:}{Object of class \code{"logical"} ~~ }
    \item{\code{is_defining_ontology}:}{Object of class \code{"logical"} ~~ }
    \item{\code{has_children}:}{Object of class \code{"logical"} ~~ }
    \item{\code{is_root}:}{Object of class \code{"logical"} ~~ }
    \item{\code{short_form}:}{Object of class \code{"character"} ~~ }
    \item{\code{obo_id}:}{Object of class \code{"NullOrChar"} ~~ }
    \item{\code{links}:}{Object of class \code{"list"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{olsLabel}{\code{signature(object = "Term")}: ... }
    \item{olsPrefix}{\code{signature(object = "Term")}: ... }
    \item{show}{\code{signature(object = "Term")}: ... }
    \item{termId}{\code{signature(object = "Term")}: ... }
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
showClass("Term")
}
\keyword{classes}
