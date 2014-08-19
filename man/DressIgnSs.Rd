\name{DressIgnSs}
\alias{DressIgnSs}
\title{Calculate Ignorance skill score, comparing two dressed ensemble forecasts.}
\description{
  Calculate the Ignorance skill score for a collection of forecast distributions generated from two competing ensemble forecasts dressed with Gaussian kernels. The standard deviation of the skill score approximated by propagation of uncertainty is provided. The higher the skill score, the higher is the improvement of `dressed.ens` over `dressed.ens.ref`.
}
\usage{
DressIgnSs(dressed.ens, dressed.ens.ref, obs)
}
\arguments{
  \item{dressed.ens}{An object of class `dressed.ens`. The ensemble forecast to be analyzed. See ?DressEnsemble for details.}
  \item{dressed.ens.ref}{An object of class `dressed.ens`. The reference forecast, that predicts the same targets as `dressed.ens` and to which `dressed.ens` is compared. See ?DressEnsemble for details.}
  \item{obs}{vector of length nrow(dressed.ens$ens). The same verifying observations for both forecasts.}
}
\value{
A list with the following elements:

"ignss": The value of the Ignorance Skill Score

"ignss.sigma": The sampling standard deviation of the skill score approximated by propagation of uncertainty.
}
\examples{
  # Example:
  ens <- matrix(rnorm(100),20,5)
  ens.ref <- ens + 0.2
  obs <- rnorm(20)
  d.ens <- DressEnsemble(ens)
  d.ens.ref <- DressEnsemble(ens.ref)
  DressIgnSs(d.ens, d.ens.ref, obs)
}

