\name{recode}
\alias{recode}
\title{Recode occupational and stratification schemas.}
\usage{
  recode(data, informat = "isco88", outformat = "esec",
    detail = 0, labels = FALSE)
}
\arguments{
  \item{data}{a single value, vector or dataframe
  containing occupational or stratification data that one
  wishes to recode. See Details.}

  \item{informat}{a string indicating the input format. See
  Details for possible values.}

  \item{outformat}{a string indicating the output format.
  See Details for possible values.}

  \item{detail}{the level of detail in returned
  classfication.  The default of 0 returns the most
  detailed form.}

  \item{labels}{return a factor with value labels instead
  of the numeric values.}
}
\description{
  \code{recode} provides an uniform function for converting
  to and from different occupational and stratification
  schemas.
}
\examples{
isco <- c(1200, 3131, 9110)
recode(isco, informat="isco88", outformat="isei")
}
\keyword{manip}

