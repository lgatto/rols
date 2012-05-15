\name{termMetadata}
\alias{termMetadata}
\title{Retuns an identifier's metadata}
\usage{
  termMetadata(termId, ontologyName, simplify = TRUE)
}
\arguments{
  \item{termId}{A \code{character} with a valid ontology
  identifier.}

  \item{ontologyName}{A \code{character} with the name of a
  valid ontology name.}

  \item{simplify}{A logical indicating whether the S4
  \code{Map} instance should be simplified. Default is
  \code{TRUE}.}
}
\value{
  A named \code{character} if \code{simplify} is
  \code{TRUE}. An instance of class \code{Map} otherwise.
}
\description{
  This function returns the metadata (definition and
  synonyms) for a specific ontology identifier. The term
  for that identifier can be retrieved with
  \code{\link{term}}. The function sends a
  \code{getTermMetadataRequest} SOAP message and retrieves
  and parses the \code{getTermMetadataResponse}. The
  original interface is \code{public Map
  getTermMetadata(String termId, String ontologyName)}.
}
\examples{
termMetadata("GO:0005794", "GO")
termMetadata("210797", "NEWT")
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
  \code{\link{term}}, \code{\link{termXrefs}}
}

