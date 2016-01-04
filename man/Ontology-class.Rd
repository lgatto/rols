\name{Ontology-class}
\Rdversion{1.1}
\docType{class}

\alias{Ontology}
\alias{class:Ontology}
\alias{Ontology-class}
\alias{Ontologies}
\alias{class:Ontologies}
\alias{Ontologies-class}

\alias{Ontologies,missing-method}
\alias{Ontologies,numeric-method}
\alias{Ontology,character-method}

\alias{olsDesc}
\alias{olsDesc,character-method}
\alias{olsDesc,Ontology-method}
\alias{olsDesc,Ontologies-method}

\alias{olsPrefix}
\alias{olsPrefix,character-method}
\alias{olsPrefix,Ontology-method}
\alias{olsPrefix,Ontologies-method}

\alias{olsStatus}
\alias{olsStatus,character-method}
\alias{olsStatus,Ontology-method}
\alias{olsStatus,Ontologies-method}

\alias{olsNamespace}
\alias{olsNamespace,character-method}
\alias{olsNamespace,Ontology-method}
\alias{olsNamespace,Ontologies-method}

\alias{olsRoot}
\alias{olsRoot,character-method}
\alias{olsRoot,Ontology-method}
\alias{olsRoot,Ontologies-method}

\alias{olsTitle}
\alias{olsTitle,character-method}
\alias{olsTitle,Ontology-method}
\alias{olsTitle,Ontologies-method}

\alias{olsVersion}
\alias{olsVersion,Ontology-method}
\alias{olsVersion,character-method}
\alias{olsVersion,Ontologies-method}

\alias{olsLoaded}
\alias{olsLoaded,Ontology-method}
\alias{olsLoaded,character-method}
\alias{olsLoaded,Ontologies-method}

\alias{olsUpdated}
\alias{olsUpdated,Ontology-method}
\alias{olsUpdated,character-method}
\alias{olsUpdated,Ontologies-method}

\alias{show,Ontology-method}
\alias{show,Ontologies-method}

\alias{length,Ontologies-method}
\alias{lapply,Ontologies-method}
\alias{[,Ontologies-method}
\alias{[[,Ontologies-method}

\alias{coerce,Ontologies,data.frame-method}
\alias{coerce,Ontologies,list-method}

\title{Class \code{"Ontology"}}

\description{

  Ontologies are stored as \code{Ontology} and \code{Ontologies}
  instances, and contain various information as provided by the Ontology
  Lookup Service. 
  
}

\section{Objects from the Class}{
  
  Objects can be created in multiple ways. The \code{Ontologies}
  function will initialise all available ontolgies as an
  \code{Ontologies} object, while a call to \code{Ontology} with an
  ontology prefix as argument will initialise the ontology of interest
  as an \code{Ontology} instance.

  \code{Ontolgies} instances can be subset with \code{[} and \code{[[}
  and iterated over with \code{lapply}. \code{Ontolgies} can be
  converted into a simple \code{data.frame} containing the ontology
  prefixes and titles using \code{as(., "data.frame")}). An
  \code{Ontologies} can also be coerced to lists of \code{Ontology}
  ojects with \code{as(., "list")}.

}

\section{Slots}{

  \describe{
    
    \item{\code{loaded}:}{Object of class \code{NULL} or
      \code{character} containing the date the ontology was loaded on
      the backend side. Accessed with the \code{olsLoaded} method.}
    
    \item{\code{updated}:}{Object of class \code{NULL} or
      \code{character} containing the date the ontology was last updated
      on the backend side. Accessed with the \code{olsUpdated}
      method.}
   
    \item{\code{status}:}{Object of class \code{NULL} or
      \code{character} documenting the status of the ontology on the
      backend side. For example \code{"LOADED"}, \code{"FAILED"} or
      \code{"NOTLOADED"}. Accessed with the \code{olsStatus} method.}
    
    \item{\code{message}:}{Object of class \code{NULL} or
      \code{character} documentating the status of the ontology on the
      backend side.  }
    
    \item{\code{version}:}{Object of class \code{NULL} or
      \code{character} documenting the version of the ontology.  Note
      that there is also a \code{version} field in the \code{config}
      slot below. Use \code{olsVersion} to access the appropriate
      date. }
    
    \item{\code{numberOfTerms}:}{Object of class \code{"integer"}
      documenting the number of terms available in the ontology. }
    
    \item{\code{numberOfProperties}:}{Object of class \code{"integer"}
      documenting the number of properties available in the ontology. }

    \item{\code{numberOfIndividuals}:}{Object of class \code{"integer"}
      documenting the number of individuals available in the ontology. }

    \item{\code{config}:}{Object of class \code{"list"} containing
      further ontology configuration and metadata. }
    
  }
}

\section{Methods and functions}{
  \describe{

    \item{Ontologies}{\code{signature(object = "numeric")}:  }

    \item{Ontology}{\code{signature(object = "character")}:  }

    
    \item{olsDesc}{\code{signature(object = "Ontology")}: returns the
      description of an ontology. Also works for \code{Ontologies}
      objects and \code{character} describing an ontology prefix. }
    
    \item{olsPrefix}{\code{signature(object = "Ontology")}: retruns the
      prefix of an ontology. Also works for \code{Ontologies} objects
      describing an ontology prefix. }
    
    \item{olsRoot}{\code{signature(object = "Ontology")}: returns the
      root of the ontology as a \code{\linkS4class{Terms}}
      instance. \code{object} could also be a \code{character} with an
      ontology prefix. If \code{object} is of class \code{Ontologies},
      it returns a \code{list} of \code{\linkS4class{Terms}}. }
    
    \item{olsVersion}{\code{signature(object = "Ontology")}: returns the
      version of the ontology. Also works with an ontology prefix as a
      \code{character} or an object of class \code{Ontologies}, in which
      case it returns a list of versions. }

    \item{olsLoaded}{\code{signature(object = "Ontology")}: returns the
      loading date of the ontology. Also works with a \code{character}
      containing the ontology prefix or an object of class
      \code{Ontologies}. }

    \item{olsUpdated}{\code{signature(object = "Ontology")}: returns the
      update date of the ontology. Also works with a \code{character}
      containing the ontology prefix or an object of class
      \code{Ontologies}.}

    \item{olsStatus}{\code{signature(object = "Ontology")}: returns the
      status of the ontology. Also works with a \code{character}
      containing the ontology prefix or an object of class
      \code{Ontologies}.}

    \item{olsStatus}{\code{signature(object = "Ontology")}: returns the
      namespace of the ontology. Also works with a \code{character}
      containing the ontology prefix or an object of class
      \code{Ontologies}.}

    \item{olsTitle}{\code{signature(object = "Ontology")}: returns the
      title of an ontology. Also works with a \code{character}
      containing the ontology prefix or an object of class
      \code{Ontologies}.}
    
    \item{show}{\code{signature(object = "Ontology")}: prints a short
      summary of \code{Ontology} and \code{Ontologies} objects. }

    \item{length}{\code{signature(object = "Ontologies")}: returns the
      number of ontolgies described by the \code{Ontologies} object. }
    
  }
}

\author{
  Laurent Gatto <lg390@cam.ac.uk>
}

\examples{
## Get all ontolgies
ol <- Ontologies()
ol

head(as(ol, "data.frame"))
length(ol)

## Individual ontologies
(go <- ol[["GO"]])
(efo <- ol[["EFO"]])

## some basic information
olsVersion(go)
olsDesc(go)
olsTitle(go)
olsPrefix(go)

olsRoot(go)

## works with Ontology objects or their prefixes
identical(olsRoot("GO"), olsRoot(go))
identical(olsVersion("GO"), olsVersion(go))

## Directly initialise a singleontology
go1 <- Ontology("go")
(GO <- Ontology("GO"))

stopifnot(identical(go, GO))
stopifnot(identical(go, go1))
}


\keyword{classes}
