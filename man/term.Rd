\name{term}
\alias{term}
\title{Returns the term of a given identifier}
\usage{
  term(termId, ontologyName)
}
\arguments{
  \item{termId}{A \code{character} with a valid ontology
  identifier.}

  \item{ontologyName}{A \code{character} with the name of a
  valid ontology name.}
}
\value{
  A \code{string} with the description of that identifier,
  as found in ontolgy \code{ontologies}. If \code{termId}
  was not found in \code{ontologies()}, as warning is
  issued and NULL is returned.
}
\description{
  This function returns the term (description) of a given
  ontology identifier in a specific ontology. The ontology
  name must be valid, i.e. exists in \code{ontologies()}.
  It sends a \code{getTermByIdRequest} SOAP message and
  retrieves and parses the \code{getTermByIdResponse}. The
  original interface is \code{public String
  getTermById(String termId, String ontologyName)}.
}
\examples{
term("GO:0005794", "GO") ## valid description
term("GO:0000000", "GO") ## returns NULL
term("210797", "NEWT")
}
\author{
  Laurent Gatto
}
\seealso{
  Other ols-queries: \code{\link{allIds}},
  \code{\link{isIdObsolete}}, \code{\link{olsQuery}},
  \code{\link{olsVersion}}, \code{\link{ontologies}},
  \code{\link{ontologyLoadDate}},
  \code{\link{ontologyNames}}, \code{\link{rootId}},
  \code{\link{termMetadata}}, \code{\link{termXrefs}}
}

