\name{as.character.Map}
\alias{as.character.Map}
\title{Coerce \code{Map} to a \code{data.frame}}
\usage{
  \method{as.character}{Map} (x, ...)
}
\arguments{
  \item{x}{An instance \code{Map}.}

  \item{...}{not used.}
}
\value{
  A \code{character} of length \code{length(map)}.
}
\description{
  \code{as} method to coerce an instance of \code{Map} to a
  \code{character}. The maps keys are used to name the map
  values.
}
\author{
  Laurent Gatto
}
\seealso{
  Other as: \code{\link{as.character.mapItem}}
}

