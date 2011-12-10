\name{esec}
\alias{esec}
\title{Recode occupational characteristics to ESeC.}
\usage{
  esec(data, details = 0, labels = FALSE)
}
\arguments{
  \item{data}{a single value, vector or dataframe
  containing occupational or stratification data that one
  wishes to recode. See Details.}

  \item{details}{a string indicating the input format. See
  Details for possible values.}

  \item{labels}{the level of detail in returned
  classfication.  The default of 0 returns the most
  detailed form.}
}
\value{
  vector of ESeC codes/labels
}
\description{
  \code{recode} provides an uniform function for converting
  to and from different occupational and stratification
  schemas.
}
\examples{
isco <- c(1200, 3131, 9110)
esec(isco, details=3, labels=TRUE)
}
\keyword{manip}

