\name{Term-class}
\Rdversion{1.1}
\docType{class}
\alias{Term}
\alias{class:Term}
\alias{Term-class}

\alias{Terms}
\alias{class:Terms}
\alias{Terms-class}

\alias{olsLabel}
\alias{olsLabel,Term-method}
\alias{olsLabel,Terms-method}

\alias{olsNamespace,Term-method}
\alias{olsNamespace,Terms-method}

\alias{olsSynonym}
\alias{olsSynonym,Term-method}
\alias{olsSynonym,Terms-method}

\alias{olsDesc,Term-method}
\alias{olsDesc,Terms-method}

\alias{olsPrefix,Term-method}
\alias{olsPrefix,Terms-method}

\alias{show,Term-method}
\alias{show,Terms-method}

\alias{termId}
\alias{termId,Term-method}
\alias{termId,Terms-method}

\alias{isObsolete}
\alias{isObsolete,Term-method}
\alias{isObsolete,Terms-method}

\alias{isRoot}
\alias{isRoot,Term-method}
\alias{isRoot,Terms-method}


\alias{terms}
\alias{term}
\alias{term,Ontology,character-method}
\alias{term,character,character-method}
\alias{terms,Ontology-method}
\alias{terms,character-method}

\alias{children}
\alias{parents}
\alias{ancestors}
\alias{descendants}

\alias{length,Terms-method}
\alias{lapply,Terms-method}
\alias{[,Terms-method}
\alias{[[,Terms-method}

\alias{all.equal,Term,Term-method}
\alias{all.equal,Terms,Terms-method}

\title{Class \code{"Term"}}

\description{

  The \code{Term} class describes an ontology term. A set of terms are
  instantiated as a \code{Terms} class.
  
}

\section{Objects from the Class}{
  
  Objects can be created using the \code{term} and \code{terms}
  functions. The latter is used with an object of class
  \code{\linkS4class{Ontology}} or a \code{character} describing a valid
  ontology prefix to download and instantiate all terms of an ontology
  of interest. The former takes an \code{Ontology} object (or an
  ontology prefix) and a term identifer to instantiate that specific
  term. See also the 'Methods and functions' sections.

  For any given \code{Term} object, the \code{children}, \code{parents},
  \code{ancestors} and \code{descendants} terms can be generated and
  returned as \code{Terms} objects.

  \code{Terms} instances can be subset with \code{[} and \code{[[} and
  iterated over with \code{lapply}.
  
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

\section{Methods and functions}{
  \describe{

    \item{term}{\code{signature(object = "Ontology", id = "character")}:
      ... }
    
    \item{terms}{\code{signature(x = "Ontology")}: ... }
    
    \item{olsLabel}{\code{signature(object = "Term")}: ... }
    
    \item{olsPrefix}{\code{signature(object = "Term")}: ... }

    \item{olsSynonym}{\code{signature(object = "Term")}: ... }

    \item{olsPrefix}{\code{signature(object = "Term")}: ... }

    \item{olsNamespace}{\code{signature(object = "Term")}: ... }

    \item{isRoot}{\code{signature(object = "Term")}: ... }

    \item{isObsolete}{\code{signature(object = "Term")}: ... }
    
    \item{termId}{\code{signature(object = "Term")}: ... }

    \item{children}{\code{signature(object = "Term")}: ... }

    \item{parents}{\code{signature(object = "Term")}: ... }
    
    \item{ancestors}{\code{signature(object = "Term")}: ... }
    
    \item{descendants}{\code{signature(object = "Term")}: ... }

    \item{show}{\code{signature(object = "Term")}: ... }

    \item{show}{\code{signature(object = "Terms")}: ... }

    \item{all.equal}{\code{signature(target = "Term", current =
	"Term")}: ... }
    
    \item{all.equal}{\code{signature(target = "Terms", current =
	"Terms")}: ... }
    
    \item{length}{\code{signature(object = "Terms")}: returns the number
      of ontolgies described by the \code{Terms} object. }

    
    
  }
  
}

\author{
  Laurent Gatto <lg390@cam.ac.uk>
}


\examples{

## (all) terms
(gotrms <- terms("go", pagesize = 10000))

\dontrun{
    ## or, using on ontology object
    go <- Ontology("go")
    gotrms <- terms(go, pagesize = 10000)
}

## (one) term
(trm <- gotrms[[1]])
olsPrefix(trm)
gotrms[1:3]
gotrms[["GO:0032801"]]


## using an Ontology object
go <- Ontology("GO")
term(go, "GO:0032801")
## using an ontology prefix
term("GO", "GO:0032801")

isObsolete(gotrms[["GO:0030533"]])
isObsolete(gotrms[["GO:0005563"]])

isRoot(gotrms[["GO:0030533"]])

i <- isRoot(gotrms) & !isObsolete(gotrms)
gotrms[i]
for (ii in which(i))
    show(gotrms[[ii]])

## or, directly querying the ontology
olsRoot(go)

parents(trm)
ancestors(trm)
children(trm)
descendants(trm)

}

\keyword{classes}
