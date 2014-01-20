\name{FairBrier}
\alias{FairBrier}
\title{Fair Brier Score for ensemble forecasts.}
\description{
Calculate the Fair Brier Score for ensemble forecasts and their corresponding observations
}
\usage{
FairBrier(ens, obs, tau)
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
  mean(FairBrier(ens, obs, tau=1))
}
\references{
Ferro, C.A.T. (2013). Fair scores for ensemble forecasts. Q.J.R. Meteorol. Soc. DOI: 10.1002/qj.2270.
}



