\name{FairCrps}
\alias{FairCrps}
\title{Fair continuously ranked probability score (CRPS) for ensemble forecasts.}
\description{
Calculate the fair continuously ranked probability score (CRPS) for ensemble forecasts and their corresponding observations.
}
\usage{
FairCrps(ens, obs)
}
\arguments{
  \item{ens}{N*K matrix. N cases of a K-member ensemble forecast.}
  \item{obs}{vector of length N. The corresponding verifying observations.}
}
\value{
  The function returns a vector of length N with the individual score values of the N ensemble forecasts.
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  mean(FairCrps(ens, obs))
}
\references{
Ferro, C.A.T. (2013). Fair scores for ensemble forecasts. Q.J.R. Meteorol. Soc. DOI: 10.1002/qj.2270.
}



