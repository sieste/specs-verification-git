\name{FairRpss}
\alias{FairRpss}
\title{Calculate fair ranked probability skill score for competing ensemble forecasts.}
\description{
  Calculate the fair ranked probability skill score (fair RPSS) between two competing ensemble forecasts for the same observation. An approximate standard deviation of the skill score is provided. The higher the skill score, the higher the improvement of `ens` over `ens.ref`.
}
\usage{
FairRpss(ens, ens.ref, obs)
}
\arguments{
  \item{ens}{N*K matrix. ens[i,j] is the number of ensemble members that predict category j at time i.}
  \item{ens.ref}{N*K matrix, similar to ens}
  \item{obs}{N*K matrix. obs[i,j] = 1 if category j is observed at time i, 0 otherwise.}
}
\value{
A list with the following elements:

"rpss": The value of the skill score.

"sigma.rpss": The standard deviation of the skill score, approximated by propagation of uncertainty.}
\examples{
  # Example:
  ens <- matrix(c(1,2,3, 1,3,2), byrow=TRUE, nrow=2)
  ens.ref <- matrix(c(2,2,2, 2,2,2), byrow=TRUE, nrow=2)
  obs <- matrix(c(0,0,1, 0,1,0), byrow=TRUE, nrow=2)
  FairRpss(ens, ens.ref, obs)
}

