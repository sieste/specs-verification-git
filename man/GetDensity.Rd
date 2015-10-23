\name{GetDensity}
\alias{GetDensity}
\title{Calculate density and integrated density function of a dressed ensemble forecast}
\description{
  The density or integrated density of the forecast distribution of a dressed ensemble is calculated at a number of x-values.
}
\usage{
GetDensity(dressed.ens, x, integrated=FALSE)
}
\arguments{
  \item{dressed.ens}{An object of class `dressed.ens`. See `?DressEns` for details.}
  \item{x}{matrix with either 1 or nrow(dressed.ens[["ens"]]) rows and an arbitrary number of columns. The x-values at which the forecast distributions are to be evaluated. See section Details.} 
  \item{integrated}{logical, (default=FALSE): If `integrated` is TRUE, the integrated density (i.e. the value of the cumulative distribution function) instead of the density is returned.}
}
\value{
  The function returns a matrix, whose rows correspond to the individual ensemble forecasts and whose columns correspond to the values provided by the argument `x`.
}
\details{
If the user whishes to evaluate the forecast distribution function of all ensembles at the same x-values, a row matrix with these x-values can be provided.

If the individual forecast distributions are to be evaluated at different x-values, a matrix with N rows must be provided, where N is the number of ensemble forecasts, where the i-th row contains the desired x-values for the i-th forecast instance.
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  dressed.ens <- DressEnsemble(ens)
  # get each density at the same x-values
  x1 <- matrix(seq(-3, 3, 0.1), nrow=1)
  dens1 <- GetDensity(dressed.ens, x1)
  # get the densities that the forecast 
  # distributions assign to the observations
  x2 <- matrix(obs, ncol=1)
  dens2 <- GetDensity(dressed.ens, x2)
  # get the integrated densities that the forecast 
  # distributions assign to the observations (useful
  # for constructing a PIT histogram)
  pit <- GetDensity(dressed.ens, x2, integrated=TRUE)
}

