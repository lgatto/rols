\name{CVParam-class}
\Rdversion{1.1}
\docType{class}
\alias{CVParam-class}
\alias{CVParam}
\alias{coerce,CVParam,character-method}
\alias{show,CVParam-method}
\alias{rep,CVParam-method}
\alias{as.character.CVParam}

\title{Class \code{"CVParam"}}

\description{
  \code{CVParam} objects instantiate controlled vocabulary entries.
}

\section{Objects from the Class}{
  Objects can be created with the \code{CVParam} constructor.
}

\usage{
CVParam(label, name, accession, value, exact = TRUE)
}

\arguments{
  \item{label}{A \code{character} with the ontology label. If missing,
    a user-defined parameter is created. }
    
  \item{name}{A \code{character} with the name of the \code{CVParam} to
    be constructed. This argument can be omitted if \code{accession} is 
    used and \code{label} is not missing. }
  
  \item{accession}{A \code{character} with the accession of the 
    \code{CVParam} to be constructed. This argument can be omitted if
    \code{name} is used. Ignored for user-defined instances. }
  
  \item{value}{A \code{character} with the value of the \code{CVParam}
    to be constructed. This argument is optional. }

  \item{exact}{A \code{logical} defining whether the query to retrieve
    the \code{accession} (when \code{name} is used) should be an
    exact match. }
    
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
    \item{coerce}{\code{signature(from = "CVParam", to = "character")}:
      Coerces \code{CVParam} \code{from} to a \code{character} of the
      following form: \code{[label, accession, name,
	value]}. \code{as.character} is also defined.
    }

    \item{show}{\code{signature(object = "CVParam")}: Prints the
      \code{CVParam} instance as text.
    }

    \item{rep}{\code{signature(x = "CVParam", times = "numeric")}:
      Replicates the \code{CVParam} \code{x} \code{times} times.       
    }
  }
}

\author{
  Laurent Gatto <lg390@cam.ac.uk>
}

\examples{
## a user param
CVParam(name = "A user param", value = "the value")
## a CVParam from PSI's Mass Spectrometry ontology
term("MS:1000073", "MS")
CVParam(label = "MS", accession = "MS:1000073")
CVParam(label = "MS", name ="electrospray ionization")
CVParam(label = "MS", name ="ESI") ## using a synonym
}


\keyword{classes}
