\name{childrenRelations}
\alias{childrenRelations}
\title{Returns the children relation type(s).}
\usage{
  childrenRelations(termId = termId,
    ontologyName = ontologyName, simplify = TRUE)
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
  This function returns the relation type of a ontology
  term \code{termId} and its children. The function sends a
  \code{getTermRelationsRequest} SOAP message and retrieves
  and parses the \code{getTermRelationsResponse}. The
  original corresponging interface is \code{public Map
  getTermRelations(String termId, String ontologyName)}.
}
\examples{
childrenRelations("GO:0005802", "GO")
}
\author{
  Laurent Gatto
}

