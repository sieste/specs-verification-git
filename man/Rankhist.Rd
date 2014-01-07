\name{Rankhist}
\alias{Rankhist}
\title{Rank histogram for ensemble forecasts}
\description{
  Calculate the rank histogram for an archive of ensemble forecasts and their corresponding verifying observations.
}
\usage{
Rankhist(ens, ver)
}
\arguments{
  \item{ens}{An archive of K-member ensemble forecasts for N time instances, saved in a matrix of dimension (N, K).}
  \item{ver}{The corresponding verifications, saved in a vector of length N.}
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  rh <- Rankhist(ens, obs)
}

