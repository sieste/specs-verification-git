\name{DressScores}
\alias{DressCrps}
\alias{DressIgn}
\title{Scores for forecast ensembles dressed with Gaussian kernels.}
\description{
  Calculate the continuously ranked probability score (CRPS) or Ignorance score of a forecast distribution created by dressing an ensemble forecast with Gaussian kernels.
}
\usage{
DressCrps(dressed.ens, obs)
DressIgn(dressed.ens, obs)
}
\arguments{
  \item{dressed.ens}{An object of class `dressed.ens`. See ?DressEnsemble for details.}
  \item{obs}{vector of length nrow(dressed.ens$ens). The verifying observations.}
}
\value{
  The function returns a vector of length N with the individual score values of the N dressed ensembles and observations.
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  d.ens <- DressEnsemble(ens)
  mean(DressCrps(d.ens, obs))
  mean(DressIgn(d.ens, obs))
}
\references{
  Gneiting, T. et al. (2007). Probabilistic forecasts, calibration and sharpness. J. R. Statist. Soc. B 69, Part 2, pp. 243--268. DOI 10.1111/j.1467-9868.2007.00587.x.
}


