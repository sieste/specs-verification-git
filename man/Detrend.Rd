\name{Detrend}
\alias{Detrend}
\title{Auxiliary function for removing trends from observation vector or ensemble matrix or data.frame.}
\description{
The function fits a linear function to a vector of observations or to the ensemble mean vector of an ensemble matrix. The linear trend is removed, without removing the mean.
}
\usage{
Detrend(x)
}
\arguments{
  \item{x}{A vector, matrix, or data.frame.}
}
\value{
  The function returns an object of the same class as the argument `x`, but with its linear trend removed.
}
\examples{
  # Example:
  ens <- matrix(1:10+rnorm(10), 5, 2)
  Detrend(ens)
  obs <- 1:10 + rnorm(10)
  Detrend(obs)
}

