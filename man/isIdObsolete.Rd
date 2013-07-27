\name{isIdObsolete}
\alias{isIdObsolete}
\title{Is the ontology id obsolete}
\usage{
  isIdObsolete(termId, ontologyName)
}
\arguments{
  \item{termId}{A \code{character} with a valid ontology
  identifier.}

  \item{ontologyName}{A \code{character} with the name of a
  valid ontology name.}
}
\value{
  A \code{logical} specifying if the term id is obsolete.
}
\description{
  When terms are found to be outside the scope of an
  ontology, are misleadingly named or defined or describe a
  concept that would be better represented in another way,
  the terms are marked obsolete rather than deleted. This
  function tests this by sending an
  \code{isObsoleteRequest} SOAP message and retrieves and
  parses the \code{isObsoleteResponse}. The original
  interface is \code{public boolean isObsolete(String
  termId, String ontologyName)}.
}
\examples{
## is obsolete
term("GO:0005563", "GO")
isIdObsolete(termId = "GO:0005563", ontologyName = "GO")
stopifnot(isIdObsolete(termId = "GO:0005563", ontologyName = "GO"))
## replaced by
term("GO:0030533", "GO")
isIdObsolete(termId = "GO:0030533", ontologyName = "GO")
}
\author{
  Laurent Gatto
}
\seealso{
  Other ols-queries: \code{\link{allIds}},
  \code{\link{olsQuery}}, \code{\link{olsVersion}},
  \code{\link{ontologies}}, \code{\link{ontologyLoadDate}},
  \code{\link{ontologyNames}}, \code{\link{rootId}},
  \code{\link{term}}, \code{\link{termMetadata}},
  \code{\link{termXrefs}}
}

