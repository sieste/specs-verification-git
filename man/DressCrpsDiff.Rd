\name{DressScoreDiff}
\alias{DressCrpsDiff}
\alias{DressIgnDiff}
\title{Calculate average score differences for competing dressed ensemble forecasts.}
\description{
  Calculate the average continuously ranked probability score (CRPS) difference or average Ignorance score difference between a collection of forecast distributions generated from two competing ensemble forecasts dressed with Gaussian kernels. Approximate sampling quantiles of the average score difference, and p-value of the paired one-sided t-test are provided. The difference between the reference forecast `dressed.ens.ref` and the forecast `dressed.ens` is calculated. The higher the score difference, the higher the improvement of `dressed.ens` over `dressed.ens.ref`.
}
\usage{
DressCrpsDiff(dressed.ens, dressed.ens.ref, obs, probs)
DressIgnDiff(dressed.ens, dressed.ens.ref, obs, probs)
}
\arguments{
  \item{dressed.ens}{An object of class `dressed.ens`. The ensemble forecast to be analyzed. See ?DressEnsemble for details.}
  \item{dressed.ens.ref}{An object of class `dressed.ens`. The reference forecast, that predicts the same targets as `dressed.ens` and to which `dressed.ens` is compared. See ?DressEnsemble for details.}
  \item{obs}{vector of length nrow(dressed.ens$ens). The same verifying observations for both forecasts.}
  \item{probs}{vector of probabilities. The probabilities of estimated sampling quantiles of the average score difference. Can be used to construct confidence intervals.}
}
\value{
A list with the following elements:

"crps.diff" or "ign.diff": The value of the average score difference.

"sampling.quantiles": The quantiles of the sampling distribution of the average score difference corresponding to the `probs` argument. The sampling quantiles are approximated by a t-distribution as follows:

    qt(probs, df=N-1) * sd(score.diff) / sqrt(N) + mean.score.diff

"p.value": p value of the one-sided paired t-test.
}
\examples{
  # Example:
  ens <- matrix(rnorm(100),20,5)
  ens.ref <- ens + 0.2
  obs <- rnorm(20)
  d.ens <- DressEnsemble(ens)
  d.ens.ref <- DressEnsemble(ens.ref)
  DressCrpsDiff(d.ens, d.ens.ref, obs, probs=c(0.05, 0.95))
  DressIgnDiff(d.ens, d.ens.ref, obs, probs=c(0.05, 0.95))
}

