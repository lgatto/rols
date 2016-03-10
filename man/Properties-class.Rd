\name{Properties-class}
\Rdversion{1.1}
\docType{class}

\alias{Properties}
\alias{class:Properties}
\alias{Properties-class}
\alias{Property}
\alias{class:Property}
\alias{Property-class}

\alias{show,Properties-method}
\alias{show,Property-method}

\alias{properties}
\alias{properties,character-method}
\alias{properties,Ontology-method}
\alias{properties,Term-method}
\alias{properties,Terms-method}

\alias{length,Properties-method}

\title{Class \code{"Properties"}}

\description{

  Propteries (relationships) between terms can be queries for complete
  \code{\linkS4class{Ontology}} objects and
  \code{\linkS4class{Term}}/\code{\linkS4class{Terms}} instances, and
  the results are stored as objects of class \code{Property} or
  \code{Properties}.
  
}

\section{Objects from the Class}{
  Objects can be created by calls to \code{properties}, as described
  below.  }

\section{Slots}{
  See the \code{\linkS4class{Term}} and \code{\linkS4class{Terms}} classes.
}

\section{Extends}{
  Class \code{"\linkS4class{Terms}"}, directly.
}

\section{Methods and functions}{
  \describe{

    \item{properties}{\code{signature(object = "Ontology", pagesize =
      200)}: ... Also works with a \code{character} with the ontology
      namespace. See \code{\link{Ontology}} for details. }

    \item{properties}{\code{signature(object = "Term")}: retrieves the
      properties of term \code{object} and returns a \code{Properties}
      object.  Returns \code{NULL} when no properties are available. }

    \item{proterties}{\code{signature(object = "Terms", ...)}: retrieves
      the properties of each term of \code{object} and returns a list of
      \code{Properties} (or \code{NULL}) items. }

    \item{show}{\code{signature(object = "Properties")}: shows a textual
      summary of the object. }

    \item{length}{\code{signature(object = "Properties")}: returns the
      number of properties in \code{object}. } } }


\author{
  Laurent Gatto <lg390@cam.ac.uk>
}

\examples{
trm <- term("uberon", "UBERON:0002107")
trm
properties(trm)

trm2 <- term("GO", "GO:0005326")
trm2
properties(trm2)
}

\keyword{classes}
