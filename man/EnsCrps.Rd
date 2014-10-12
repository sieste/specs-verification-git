\name{EnsCrps}
\alias{EnsCrps}
\title{Continuously ranked probability score (CRPS) for ensemble forecasts.}
\description{
Calculate the continuously ranked probability score (CRPS) for ensemble forecasts and their corresponding observations. The forecast distribution is approximated by a step function with equal weights on the ensemble members.
}
\usage{
EnsCrps(ens, obs)
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
  mean(EnsCrps(ens, obs))
}
\references{
Hersbach, H (2000). Decomposition of the Continuous Ranked Probability Score for Ensemble Prediction Systems. Weather and Forecasting. DOI: 10.1175/1520-0434(2000)015<0559:DOTCRP>2.0.CO;2
}



