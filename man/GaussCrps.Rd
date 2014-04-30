\name{GaussCrps}
\alias{GaussCrps}
\title{Continuously ranked probability score (CRPS) for probabilistic forecasts issued as Gaussian distributions.}
\description{
Calculate the continuously ranked probability score (CRPS) for Gaussian forecasts and their corresponding observations.
}
\usage{
GaussCrps(mean, sd, obs)
}
\arguments{
  \item{mean}{vector of length N. The forecast means.}
  \item{sd}{vector of length N. The forecast standard deviations.}
  \item{obs}{vector of length N. The corresponding verifying observations.}
}
\value{
  The function returns a vector of length N with the individual score values of the N forecasts.
}
\examples{
  # Example:
  m <- rnorm(100)
  s <- runif(100) + 1
  o <- rnorm(100, m, s)
  mean(GaussCrps(m, s, o))
}
\references{
Gneiting et al (2005). Calibrated Probabilistic Forecasting Using Ensemble Model Output Statistics and Minimum CRPS Estimation. Mon. Wea. Rev. DOI: 10.1175/MWR2904.1
}



