\name{BrierScoreDecomposition}
\alias{BrierScoreDecomposition}
\title{Brier Score Decomposition}
\description{
  Decomposition of the empirical Brier Score into its Reliability, Resolution, and Uncertainty component.
}
\usage{
BrierScoreDecomposition(p, y, calibration=list(method="bin", bins=10),
probs=NA, n.boot=0)
}
\arguments{
  \item{p}{vector of length N. The forecast probabilities to analyze.}
  \item{y}{vector of length N. The corresponding binary events (0 = no event, 1 = event)}
  \item{calibration}{list. Specifies the method to estimate the calibration function. See Details.}
  \item{probs}{probability values for which quantiles are to be estimated by the bootstrap}
  \item{n.boot}{number of bootstrap replicates}
}
\value{
If `n.boot=0` or `probs=NA`: A 3 element vector containing the components Reliability (REL), Resolution (RES) and Uncertainty (UNC) of the decomposition. See Details.
Otherwise: A list, containing a 3 element vector with the components of the decomposition, and a matrix with 3 columns containing the requested bootstrap quantiles for REL, RES, and UNC.
}
\details{
The expectation value of the Brier Score of a forecast `p` for the binary event `y` is given by

Br = E[(y-p)^2]

Define the calibration function F and the climatological probability C as

F(p) = P(y=1|p), and C = P(y=1).

Then the decomposition into the three nonnegative terms REL, RES and UNC is written as

Br = REL - RES + UNC

where

REL = E[(p - F(p))^2]\cr
RES = E[(F(p) - C)^2]\cr
UNC = C*(1-C)

The calibration function F(p) must be estimated from the data `y` and `p`, for which the following methods are implemented:

"bin": The standard method of dividing the unit interval into a number of bins and estimating F(p) by `NumberOf(y==1 and p in bin i) / NumberOf(p in bin i)`. If `calibration=list(method="bin",bins=10)` (the default), the unit interval is divided into 10 equidistant bins. The argument `bins` can also be a vector of break points of the desired bins, such as `calibration=list(method=bin, bins=c(0, 0.2, 0.6, 1)`. For the latter, the first and last element of `bins` have to be 0 and 1, respectively.

"logistic": The calibration function is estimated by fitting a logistic regression model to the binary verifications `y`, using `p` as the predictor.
}
\examples{
  # Example:
  p <- rbeta(n=100, shape1=1, shape2=3)
  y <- rbinom(n=100, size=1, prob=p^0.9)
  BrierScoreDecomposition(p, y, calibration=list(method="bin", bins=5))
}
\references{
A.H. Murphy (1973). A new vector partition of the probability score. J. Appl. Met., 12:595--600. doi: 10.1175/1520-0450(1973)012<0595:ANVPOT>2.0.CO;2.

J. Broecker. Reliability, sufficiency, and the decomposition of proper scores. Q. J. Roy. Met. Soc., 135(643):1512--1519, 2009. doi: 10.1002/qj.456.
}


