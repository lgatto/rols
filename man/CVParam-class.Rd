\name{CVParam-class}
\Rdversion{1.1}
\docType{class}
\alias{CVParam-class}
\alias{CVParam}
\alias{coerce,CVParam,character-method}
\alias{coerce,character,CVParam-method}
\alias{show,CVParam-method}
\alias{rep,CVParam-method}
\alias{as.character.CVParam}
\alias{as.CVParam.character}
\alias{charIsCVParam}

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
      the label of the instance, i.e the ontology
      abbreviation/prefix. See \code{\link{Ontologies}} to generate a
      list of available ontologies and \code{\link{olsPrefix}} for
      existing labels.}

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

    \item{charIsCVParam(x)}{Checks if \code{x}, a character of the form
      \code{"[ONTO, ACCESSION, NAME, VALUE]"}, is a valid (possibly
      user-defined) \code{CVParam}. \code{"ONTO"} is the ontology label
      (prefi), \code{"ACCESSION"} is the term accession number,
      \code{"NAME"} is the term's name and \code{"VALUE"} is the
      value. Note that only syntax validity is verified, not semantics.
      See example below.

    }
  }
}

\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "CVParam", to = "character")}:
      Coerces \code{CVParam} \code{from} to a \code{character} of the
      following form: \code{[label, accession, name,
        value]}. \code{as.character} is also defined.
    }

    \item{coerce}{\code{signature(from = "character", to = "CVParam")}:
      Coerces \code{character} \code{from} to a
      \code{CVParam}. \code{as.CVParam} is also defined. If a
      \code{label} is absent, the \code{character} is converted to a
      User param, else, the \code{label} and \code{accession} are used
      to query the Ontology Lookup Service (see
      \code{\link{OlsSearch}})). If a \code{name} is provided and does
      not match the retrieved name, a warning is thrown.

      This function is vectorised; if the \code{from} character is of
      length greater than 1, then a list of \code{CVParam} is
      returned. The queries to the OLS are processed one-by-one, though.

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
term("MS", "MS:1000073")
CVParam(label = "MS", accession = "MS:1000073")

## From a CVParam object to a character
cv <- as(CVParam(label = "MS", accession = "MS:1000073"), "character")
cv

## From a character object to a CVParam
as(cv, "CVParam")
as("[MS, MS:1000073, , ]", "CVParam") ## no name
as("[MS, MS:1000073, ESI, ]", "CVParam") ## name does not match
as(c(cv, cv), "CVParam") ## more than 1 character

x <- c("[MS, MS:1000073, , ]", ## valid CV param
       "[, , Hello, world]",   ## valid User param
       "[this, one is, not, valid]", ## not valid
       "[ , , , ]") ## not valid

stopifnot(charIsCVParam(x) == c(TRUE, TRUE, FALSE, FALSE))

## A list of expected valid and non-valid entries
rols:::validCVchars
rols:::notvalidCVchars
}


\keyword{classes}
