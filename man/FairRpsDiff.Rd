\name{FairRpsDiff}
\alias{FairRpsDiff}
\title{Calculate average score differences for competing ensemble forecasts.}
\description{
  Calculate the average fair ranked probability score (fair RPS) difference between two competing ensemble forecasts for the same observation. Approximate sampling quantiles of the average score difference are provided. The difference between the score of `ens.ref` and `ens` is calculated. The higher the score difference, the higher the improvement of `ens` over `ens.ref`.
}
\usage{
FairRpsDiff(ens, ens.ref, obs, probs)
}
\arguments{
  \item{ens}{N*K matrix. ens[i,j] is the number of ensemble members that predict category j at time i.}
  \item{ens.ref}{N*K matrix, similar to ens}
  \item{obs}{N*K matrix. obs[i,j] = 1 if category j is observed at time i, 0 otherwise.}
  \item{probs}{vector of probabilities. The probabilities of estimated sampling quantiles of the average score difference. Can be used to construct confidence intervals.}
}
\value{
A list with the following elements:

"rps.diff": The value of the average score difference.

"sampling.quantiles": The quantiles of the sampling distribution of the average score difference corresponding to the `probs` argument. The sampling quantiles are approximated by a t-distribution as follows:

    qt(probs, df=N-1) * sd(score.diff) / sqrt(N) + mean.score.diff
}
\examples{
  # Example:
  ens <- matrix(c(1,2,3, 1,3,2), byrow=TRUE, nrow=2)
  ens.ref <- matrix(c(2,2,2, 2,2,2), byrow=TRUE, nrow=2)
  obs <- matrix(c(0,0,1, 0,1,0), byrow=TRUE, nrow=2)
  FairRpsDiff(ens, ens.ref, obs, probs=c(0.025, 0.975))
}

