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
  Description comes here
}

\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("Properties", ...)}.
  %%  ~~ describe objects here ~~ 
}

\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"list"} ~~ }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{Terms}"}, directly.
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Properties")}: ... }

    \item{length}{\code{signature(object = "Properties")}: ... }
  }
}


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
