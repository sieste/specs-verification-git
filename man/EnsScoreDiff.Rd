\name{EnsScoreDiff}
\alias{EnsCrpsDiff}
\alias{EnsBrierDiff}
\title{Calculate average score differences for competing ensemble forecasts.}
\description{
  Calculate the average continuously ranked probability score (CRPS) difference or Brier Score difference between two competing ensemble forecasts for the same observation. Approximate sampling quantiles of the average score difference and p-value of the paired one-sided t-test are provided. The score difference between `ens.ref` and `ens` is calculated. The higher the score difference, the higher the improvement of `ens` over `ens.ref`.
}
\usage{
EnsCrpsDiff(ens, ens.ref, obs, probs)
EnsBrierDiff(ens, ens.ref, obs, tau, probs)
}
\arguments{
  \item{ens}{N*K matrix. A collection of N ensemble forecasts, each with K members.}
  \item{ens.ref}{A N*L matrix. A collection of N ensemble forecasts, each with L members, which predict the same observation as `ens`.}
  \item{obs}{vector of length N. The verifying observations that `ens` and `ens.ref` try to predict.}
  \item{tau}{The threshold whose exceedance defines the binary event for the Brier Score.}
  \item{probs}{vector of probabilities. The probabilities of estimated sampling quantiles of the average score difference. Can be used to construct confidence intervals.}
}
\value{
A list with the following elements:

"crps.diff" or "br.diff": The value of the average score difference.

"sampling.quantiles": The quantiles of the sampling distribution of the average score difference corresponding to the `probs` argument. The sampling quantiles are approximated by a t-distribution as follows:

    qt(probs, df=N-1) * sd(score.diff) / sqrt(N) + mean.score.diff

"p.value": p value of the one-sided paired t-test.
}
\examples{
  # Example:
  ens <- matrix(rnorm(100),20,5)
  ens.ref <- ens[, 1:3] + 0.2
  obs <- rnorm(20)
  EnsCrpsDiff(ens, ens.ref, obs, probs=c(0.05, 0.95))
  EnsBrierDiff(ens, ens.ref, obs, tau=1, probs=c(0.05, 0.95))
}

