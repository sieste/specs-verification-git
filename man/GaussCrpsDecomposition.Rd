\name{GaussCrpsDecomposition}
\alias{GaussCrpsDecomposition}
\title{Decomposition of the continuously ranked probability score for Gaussian forecasts into Reliability, Resolution and Uncertainty}
\description{
  Decomposition of the continuously ranked probability score (CRPS) for Gaussian forecasts into Reliability, Resolution, and Uncertainty.
}
\usage{
  GaussCrpsDecomposition(mean, sd, obs, probs, n.boot)
}
\arguments{
  \item{mean}{vector of length N. The forecast means.}
  \item{sd}{vector of length N. The forecast standard deviations.}
  \item{obs}{vector of length N. The corresponding observations.}
  \item{probs}{probability values for which quantiles are to be estimated by the bootstrap}
  \item{n.boot}{number of bootstrap replicates}
}
\value{
A list with the following elements:
    REL, RES, and UNC: the estimated components of the CRPS decomposition
    CRPS: the average CRPS of the forecasts
    CRPS.clim: the average CRPS of the forecast given by the climatological mean and standard deviation
    CRPS.cal: the average CRPS of the forecast after calibration by Nonhomogeneous Gaussian Regression (NGR, see Details).
    ngr.par: 4 element vector containing the NGR parameters.
    BOOT.quantiles: A matrix with 6 columns containing the requested bootstrap quantiles for REL, RES, UNC, CRPS, CRPS.clim, and CRPS.cal.
}
\details{
Decomposition of verification scores requires an estimation of the climatological distribution of the observations, and a calibrated forecast, defined as the conditional distribution of the observation, given the forecast. The climatological distribution is estimated by a Gaussian with mean and variance equal to the mean and variance of the observations. The calibration function is estimated by a parametric model called Nonhomogeneous Gaussian Regression (NGR, Gneiting 2005). Given the forecast mean and standard deviation at time t, m(t) and s(t), the calibrated mean m.cal(t) and calibrated standard deviation s.cal(t) are given by

m.cal(t) = a + b   * m(t)\cr
s.cal(t) = c + d^2 * s(t)\cr

where a, b, c, d are constant parameters estimated by minimum CRPS estimation using the R function `optim` with option `method="BFGS"`.

Then the decomposition of the CRPS into the three nonnegative terms REL, RES and UNC is written as

CRPS = REL - RES + UNC

where REL, RES and UNC are defined in terms of the divergence function d(F,G) and entropy function e(F)

REL = mean[d(F, F.cal)]\cr
RES = mean[d(F.clim, F.cal)]\cr
UNC = e(F.clim)

which are defined by the CRPS scoring rule. For more mathematical details see Siegert (2014).
}
\examples{
  # Example:
  m <- rnorm(100)
  s <- runif(100) + 1
  o <- rnorm(100, m, s)
  # in application, use a higher n.boot than 10! 
  GaussCrpsDecomposition(mean=m, sd=s, obs=o, probs=c(0.05,0.95),n.boot=10)
}
\references{
Gneiting et al (2005). Calibrated Probabilistic Forecasting Using Ensemble Model Output Statistics and Minimum CRPS Estimation. Mon. Wea. Rev. DOI: 10.1175/MWR2904.1\cr
Siegert et al (2014). Decomposition of the continuously ranked probability score for Gaussian forecasts. In preparation; contact the author for a draft.\cr
}


