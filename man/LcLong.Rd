\name{LcLong}
\alias{LcLong}
\title{Format Lorenz curve data for plotting}
\usage{
  LcLong(Lorenz)
}
\arguments{
  \item{Lorenz}{A list of objects created by
  \code{\link{Lc}}, preferably named.}
}
\value{
  A data frame with values mapped to x and y, appropriate
  for passing into \code{\link{ggplot}}.
}
\description{
  Normalize formats across Lc objects to make plot handling
  a little easier. Theoretical Lorenz curves can be passed
  in but only if accompanied by one Lc object.
}

