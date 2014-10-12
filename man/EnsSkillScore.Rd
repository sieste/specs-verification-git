\name{EnsSkillScore}
\alias{EnsCrpss}
\alias{EnsBrierSs}
\title{Calculate Brier skill score or continous ranked probability skill score for competing ensemble forecasts.}
\description{
  Calculate Brier skill score or continuous ranked probability skill score between two competing ensemble forecasts for the same observation. The sampling standard deviation is estimated by propagation of uncertainty. The higher the skill score, the higher the improvement of `ens` over `ens.ref`.
}
\usage{
EnsBrierSs(ens, ens.ref, obs, tau)
EnsCrpss(ens, ens.ref, obs)
}
\arguments{
  \item{ens}{N*K matrix. A collection of N ensemble forecasts, each with K members.}
  \item{ens.ref}{A N*L matrix. A collection of N ensemble forecasts, each with L members, which predict the same observation as `ens`.}
  \item{obs}{vector of length N. The verifying observations that `ens` and `ens.ref` try to predict.}
  \item{tau}{The threshold whose exceedance defines the binary event for the Brier Score.}
}
\value{
A list with the following elements:

"bss" or "cprss": The value of the skill score, calculated by 1 - score.of.ens / score.of.ens.ref

"crpss.sigma": The standard deviation of the skill score approximated by propagation of uncertainty.
}
\examples{
  # Example:
  ens <- matrix(rnorm(100),20,5)
  ens.ref <- ens[, 1:3] + 0.2
  obs <- rnorm(20)
  EnsCrpss(ens, ens.ref, obs)
  EnsBrierSs(ens, ens.ref, obs, tau=1)
}

