\name{CVParam-class}
\Rdversion{1.1}
\docType{class}
\alias{CVParam-class}
\alias{CVParam}
\alias{coerce,CVParam,character-method}
\alias{show,CVParam-method}
\alias{as.character.CVParam}

\title{Class \code{"CVParam"}}

\description{
  \code{CVParam} objects instantiate controlled vocabulary entries.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CVParam",
  versions, ...)}. An automated  constructer will be added later. 

}
\section{Slots}{
  \describe{
    \item{\code{label}:}{Object of class \code{"character"} that defines
    the label of the instance, i.e the ontology abbreviation. See
    \code{\link{ontologyNames}} for a complete list. }
    \item{\code{accession}:}{Object of class \code{"character"} with the
      parameter's valid \code{label} ontology accession number. See
      below for validity constrains. }
    \item{\code{name}:}{ Object of class \code{"character"} with the
      instance's valid name, i.e matching with the
      \code{accession}. \code{name} and \code{accession} must follow
      \code{term(accession, label) == name} for the instance to be
      valid. } 
    \item{\code{value}:}{Object of class \code{"character"} with the
      \code{CVParams} value, if applicable, of empty string ("")
      otherwise. }
    \item{\code{user}:}{Object of class \code{"logical"} defining if the 
      instance is a user-defined parameter (also called User params). }
    \item{\code{.__classVersion__}:}{Object of class
      \code{"\linkS4class{Versions}"} describing the instance's class
      definition version. For development use. }
  }
}

\section{Extends}{
  Class \code{"\linkS4class{Versioned}"}, directly.
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "CVParam", to = "character")}:  }
    \item{show}{\code{signature(object = "CVParam")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
  Laurent Gatto <lg390@cam.ac.uk>
}

\examples{
## a user param
new("CVParam", user = TRUE, name = "A user param", value = "the value")
## a CVParam from PSI's Mass Spectrometry ontology
term("PSI:1000073", "PSI")
new("CVParam", label = "PSI", accession = "PSI:1000073", name ="Electrospray Ionization")
}


\keyword{classes}
