\name{Corr}
\alias{Corr}
\title{Correlation coefficient of the ensemble mean.}
\description{
Calculate the Pearson correlation coefficient between the mean ensemble forecast and the corresponding observations. A confidence interval and p-value of a one-sided t-test are provided.
}
\usage{
Corr(ens, obs, probs=c(0.025, 0.975))
}
\arguments{
  \item{ens}{N*K matrix. N cases of a K-member ensemble forecast.}
  \item{obs}{vector of length N. The corresponding verifying observations.}
  \item{probs}{vector of probabilities for which the quantiles of the sampling distributions are calculated, based on Fisher's z-transform}
}
\value{
  The function returns a vector of length `2 + length(probs)`. The first element is the correlation coefficient of the ensemble mean. The last element is the p-value of the one-sided t-test based on the Fisher z transform (see von Storch and Zwiers 1999, p.149). The central elements correspond to the confidence limits given by `probs`, calculated using the Fisher z transform (see von Storch and Zwiers 1999, p.148).
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  Corr(ens, obs, probs=c(0.05, 0.95))
}
\references{
von Storch and Zwiers (1999). Statistical Analysis in Climate Research. Cambridge University Press.
}

