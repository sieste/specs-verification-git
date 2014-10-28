\name{Detrend}
\alias{Detrend}
\title{Auxiliary function for removing trends and mean from observation vector or ensemble matrix or data.frame.}
\description{
The function fits a linear function to a vector of observations or to the ensemble mean vector of an ensemble matrix. The linear trend is removed, and if option demean is true, the mean grand mean is removed as well.
}
\usage{
Detrend(x, demean=TRUE)
}
\arguments{
  \item{x}{A vector, matrix, or data.frame.}
  \item{demean}{logical; if true, the grand mean is removed from x}
}
\value{
  The function returns an object of the same class as the argument `x`, but with its linear trend (and possibly its mean) removed.
}
\examples{
  # Example:
  ens <- matrix(1:10+rnorm(10), 5, 2)
  Detrend(ens)
  obs <- 1:10 + rnorm(10)
  Detrend(obs, demean=FALSE)
}

