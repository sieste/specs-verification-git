\name{GaussCrpsDiff}
\alias{GaussCrpsDiff}
\title{Calculate average score differences for competing probabilistic forecasts.}
\description{
 Calculate the average continuously ranked probability score (CRPS) difference between two competing probabilistic forecasts issued as Gaussian distributions for the same observation. Approximate sampling quantiles of the average score difference and p-value of the paired one-sided t-test are provided. The difference between the reference forecast (mean.ref, sd.ref) and the forecast (mean, sd) is calculated. The higher the score difference, the higher the improvement of (mean, sd) over (mean.ref, sd.ref).
}
\usage{
GaussCrpsDiff(mean, sd, mean.ref, sd.ref, obs, probs)
}
\arguments{
  \item{mean, mean.ref}{vectors of length N. The means of the forecast and the reference forecast.}
  \item{sd, sd.ref}{vectors of length N. The standard deviations of the forecast and the reference forecast.}
  \item{obs}{vector of length N. The verifying observations that the two forecasts try to predict.}
  \item{probs}{vector of probabilities. The probabilities of estimated sampling quantiles of the average score difference. Can be used to construct confidence intervals.}
}
\value{
A list with the following elements:

"crps.diff": The value of the average score difference.

"sampling.quantiles": The quantiles of the sampling distribution of the average score difference corresponding to the `probs` argument. The sampling quantiles are approximated by a t-distribution as follows:

    qt(probs, df=N-1) * sd(score.diff) / sqrt(N) + mean.score.diff

"p.value": p value of the one-sided paired t-test.
}
\examples{
  # Example:
  m <- rnorm(100)
  s <- runif(100) + 1
  o <- rnorm(100, m, s)
  m.ref <- mean(o)
  s.ref <- sd(o)
  GaussCrpsDiff(m, s, m.ref, s.ref, o, probs=c(0.05, 0.95))
}

