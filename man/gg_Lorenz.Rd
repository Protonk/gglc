\name{gg_Lorenz}
\alias{gg_Lorenz}
\title{Plot formatted Lorenz Curves}
\usage{
  gg_Lorenz(lor.df, alpha = 1, eq.ray = FALSE,
    ray.col = "blue", outline.col = "black")
}
\arguments{
  \item{lor.df}{A data frame with the same format as
  produced by \code{\link{LcLong}}.}

  \item{alpha}{Numeric describing the desired alpha for
  each polygon}

  \item{eq.ray}{Logical TRUE if we want the 45 degree ray
  from the origin displayed}

  \item{ray.col}{Color for the ray from the origin}

  \item{outline.col}{Color for the box outline.}
}
\value{
  A \code{\link{ggplot}} object.
}
\description{
  Plut one or multiple Lorenz curves using ggplot2
}

