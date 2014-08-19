\name{GaussCrpss}
\alias{GaussCrpss}
\title{Calculate CRPSS for competing Gaussian probabilistic forecasts.}
\description{
 Calculate the continuously ranked probability skill score (CRPS) between two competing probabilistic forecasts issued as Gaussian distributions for the same observation. The standard deviation of the skill score is approximated by propagation of uncertainty. The higher the skill score, the higher the improvement of (mean, sd) over (mean.ref, sd.ref).
}
\usage{
GaussCrpss(mean, sd, mean.ref, sd.ref, obs)
}
\arguments{
  \item{mean, mean.ref}{vectors of length N. The means of the forecast and the reference forecast.}
  \item{sd, sd.ref}{vectors of length N. The standard deviations of the forecast and the reference forecast.}
  \item{obs}{vector of length N. The verifying observations that the two forecasts try to predict.}
}
\value{
A list with the following elements:

"crpss": The value of the skill score.

"sigma.crpss": The standard deviation of the skill score.
}
\examples{
  # Example:
  m <- rnorm(100)
  s <- runif(100) + 1
  o <- rnorm(100, m, s)
  m.ref <- mean(o)
  s.ref <- sd(o)
  GaussCrpss(m, s, m.ref, s.ref, o)
}

