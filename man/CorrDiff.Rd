\name{CorrDiff}
\alias{CorrDiff}
\title{Calculate and evaluate the difference of correlation of two ensemble forecasts.}
\description{
  Calculate the difference of the correlation coefficients of two competing ensemble forecasts for the same observation. Confidence intervals and a p value of a one-sided test for equality are calculated to evaluate the difference. 
}
\usage{
CorrDiff(ens, ens.ref, obs, sign.level=0.05)
}
\arguments{
  \item{ens}{N*K matrix. A collection of N ensemble forecasts, each with K members.}
  \item{ens.ref}{A N*L matrix. A collection of N ensemble forecasts, each with L members, which predict the same observation as `ens`.}
  \item{obs}{vector of length N. The verifying observations that `ens` and `ens.ref` try to predict.}
  \item{sign.level}{The significance level of the confidence interval of the correlation difference.}
}
\value{
The function returns a vector of length 4. The first element is the difference of the correlation coefficients `cor(ens, obs) - cor(ens.ref, obs)`. The second and third element are the lower and upper confidence limits corresponding to `sign.level`, calculated using results from Zou (2007). The 4th element is a p-value of a one-sided t-test for zero difference, calculated using the result from Steiger (1980).
}
\examples{
  # Example:
  ens <- matrix(rnorm(100),20,5)
  ens.ref <- ens[, 1:3] + 0.2
  obs <- rnorm(20)
  CorrDiff(ens, ens.ref, obs, sign.level=0.1)
}
\references{
Steiger, JH (1980). Tests for Comparing Elements of a Correlation Matrix. Psychological Bulletin Vol. 87, No. 2, pp 245-251

Zou, GY (2007). Toward Using Confidence Intervals to Compare Correlation. Psychological Methods Vol. 12, No. 4, pp 399-413
}

