\name{termXrefs}
\alias{termXrefs}
\title{Returns the idenifier's ontology cross references}
\usage{
  termXrefs(termId, ontologyName, simplify = TRUE)
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
  This function returns ontology cross references for for
  an identifier.  The function sends a
  \code{getTermXrefsRequest} SOAP message and retrieves and
  parses the \code{getTermXrefsResponse}. The original
  interface is \code{public Map getTermXrefs(String termId,
  String ontologyName)}.
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
  \code{\link{term}}, \code{\link{termMetadata}}
}

