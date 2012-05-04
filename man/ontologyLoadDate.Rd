\name{ontologyLoadDate}
\alias{ontologyLoadDate}
\title{Returns the ontology load date}
\usage{
  ontologyLoadDate(ontologyName)
}
\arguments{
  \item{ontologyName}{A \code{character} with the name of a
  valid ontology name.}
}
\value{
  A \code{character} with the ontology's load date.
}
\description{
  This function returns the load date of a given ontology.
  The ontology name must be valid, i.e. exists in
  \code{ontologies()}. It sends a
  \code{getOntologyLoadDateRequest} SOAP message and
  retrieves and parses the
  \code{getOntologyLoadDateResponse}. The original
  interface is \code{public String
  getOntologyLoadDate(String ontologyName)}.
}
\examples{
ontologyLoadDate("GO")
ontologyLoadDate("FIX")
}
\author{
  Laurent Gatto
}
\seealso{
  Other ols-queries: \code{\link{allIds}},
  \code{\link{isIdObsolete}}, \code{\link{olsQuery}},
  \code{\link{olsVersion}}, \code{\link{ontologies}},
  \code{\link{ontologyNames}}, \code{\link{rootId}},
  \code{\link{term}}, \code{\link{termMetadata}},
  \code{\link{termXrefs}}
}

