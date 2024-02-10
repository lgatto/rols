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
\alias{Ontology,Ontology-method}
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

\alias{olsLinks}
\alias{olsLinks,Ontology-method}

\alias{show,Ontology-method}
\alias{show,Ontologies-method}

\alias{length,Ontologies-method}
\alias{lapply,Ontologies-method}
\alias{[,Ontologies-method}
\alias{[[,Ontologies-method}

\alias{coerce,Ontologies,data.frame-method}
\alias{coerce,Ontologies,list-method}

\alias{all.equal,Ontologies,Ontologies-method}
\alias{all.equal,Ontology,Ontology-method}

\title{Class \code{"Ontology"}}

\description{

  The rols package provides an interface to PRIDE's Ontology Lookup
  Servive (OLS) and can be used to query one or multiple ontologies,
  stored as \code{Ontology} and \code{Ontologies} instances, and
  containing various information as provided by OLS.
}

\details{

  Ontologies are referred to by their namespace, which is lower case:
  the Gene Onology is "go", the Mass spectrometry ontology is "ms",
  etc. The ontologies also have prefixes, which are upper case: the Gene
  Onology prefix "GO", the Mass spectrometry ontology prefix "MS". One
  exception to this rule is the Drosophila Phenotype Ontology, whose
  namespace and prefix are "dpo" and "FBcv" respectively (there might be
  more). This is particularly confusing as the FlyBase Controlled
  Vocabulary has "fbcv" and "FBcv" as namespace and prefix respectively.

  When using a character to initialise an ontology or query a term,
  "fbcv" (this is case insensitive) will refer to the the FlyBase
  Controlled Vocabulary. The the Drosophila Phenotype Ontology will have
  to be referred as "dpo" (also case insensitive).

}

\section{Objects from the Class}{

  Objects can be created in multiple ways. The \code{Ontologies}
  function will initialise all available ontolgies as an
  \code{Ontologies} object, while a call to \code{Ontology} with an
  ontology namespace or prefix (but see Details section) as argument
  will initialise the ontology of interest as an \code{Ontology}
  instance.

  \code{Ontolgies} instances can be subset with \code{[} and \code{[[}
  (using their namespace, see Details) and iterated over with
  \code{lapply}. \code{Ontolgies} can be converted into a simple
  \code{data.frame} containing the ontology prefixes, namespaces and
  titles using \code{as(., "data.frame")}). An \code{Ontologies} can
  also be coerced to lists of \code{Ontology} ojects with \code{as(.,
  "list")}.

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
      objects and \code{character} describing an ontology namespace or
      prefix (see Details). }

    \item{olsPrefix}{\code{signature(object = "Ontology")}: retruns the
      prefix of an ontology. Also works for \code{Ontologies} objects
      describing an ontology namespace or prefix (see Details). }

    \item{olsRoot}{\code{signature(object = "Ontology")}: returns the
      root of the ontology as a \code{\linkS4class{Terms}}
      instance. \code{object} could also be a \code{character} with an
      ontology namespace or prefix (see Details). If \code{object} is of
      class \code{Ontologies}, it returns a \code{list} of
      \code{\linkS4class{Terms}}. }

    \item{olsVersion}{\code{signature(object = "Ontology")}: returns the
      version of the ontology. Also works with an ontology namespace or
      prefix (see Details) as a \code{character} or an object of class
      \code{Ontologies}, in which case it returns a list of versions. }

    \item{olsLoaded}{\code{signature(object = "Ontology")}: returns the
      loading date of the ontology. Also works with a \code{character}
      containing the ontology namespace or prefix (see Details) or an
      object of class \code{Ontologies}. }

    \item{olsUpdated}{\code{signature(object = "Ontology")}: returns the
      update date of the ontology. Also works with a \code{character}
      containing the ontology namespace or prefix (see Details) or an
      object of class \code{Ontologies}.}

    \item{olsStatus}{\code{signature(object = "Ontology")}: returns the
      status of the ontology. Also works with a \code{character}
      containing the ontology namespace or prefix (see Details) or an
      object of class \code{Ontologies}.}

    \item{olsTitle}{\code{signature(object = "Ontology")}: returns the
      title of an ontology. Also works with a \code{character}
      containing the ontology namespace or prefix (see Details) or an
      object of class \code{Ontologies}.}

    \item{show}{\code{signature(object = "Ontology")}: prints a short
      summary of \code{Ontology} and \code{Ontologies} objects. }

    \item{length}{\code{signature(object = "Ontologies")}: returns the
      number of ontolgies described by the \code{Ontologies} object. }

    \item{all.equal}{\code{signature(target = "Ontologies", current =
        "Ontologies")}: ... }

  }
}

\author{
  Laurent Gatto <laurent.gatto@uclouvain.be>
}

\examples{
## Get all ontolgies
ol <- Ontologies()
ol

head(as(ol, "data.frame"))
length(ol)

## Individual ontologies
(efo <- ol[["efo"]])
(go <- ol[["go"]])
(go2 <- Ontology("go"))
identical(go, go2)

## some basic information
olsVersion(go)
olsDesc(go)
olsTitle(go)
olsPrefix(go)
olsNamespace(go)

## with Ontology objects or their namespace
identical(olsVersion("go"), olsVersion(go))

## Directly initialise a single ontology
go1 <- Ontology("go") ## using the namespace (preferred)
go2 <- Ontology("GO") ## using the prefix (see Details)
all.equal(go, go1)
all.equal(go, go2)
}


\keyword{classes}
