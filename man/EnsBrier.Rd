\name{EnsBrier}
\alias{EnsBrier}
\title{Brier Score for ensemble forecasts.}
\description{
Calculate the Brier Score for ensemble forecasts and their corresponding observations
}
\usage{
EnsBrier(ens, obs, tau)
}
\arguments{
  \item{ens}{N*K matrix. N cases of a K-member ensemble forecast.}
  \item{obs}{vector of length N. The corresponding verifying observations.}
  \item{tau}{The threshold value whose exceedance defines the binary event. If ensemble members and observations are coded as 0's and 1's already, setting tau=0.5 (the default) will produce the desired result.}
}
\value{
  The function returns a vector of length N with the individual score values of the N ensemble forecasts.
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  mean(EnsBrier(ens, obs, tau=1))
}
\references{
Brier, G. (1950). Verification of forecasts expressed in terms of probability. Monthly Weather Review. DOI: 10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2
}



