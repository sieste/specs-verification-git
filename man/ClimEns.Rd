\name{ClimEns}
\alias{ClimEns}
\title{Construct a climatological ensemble from a vector of observations.}
\description{
Construct a climatological ensemble from a vector of observations. Optionally, the climatological ensemble observation at time t can be constructed without the observation at time t (leave-one-out).
}
\usage{
ClimEns(obs, leave.one.out=TRUE)
}
\arguments{
  \item{obs}{vector of length N. The observations.}
  \item{leave.one.out}{logical, default=TRUE. Whether the n-th observation should be removed from the n-th row of the ensemble matrix.}
}
\value{
  The function returns a matrix with N rows and N-1 columns (if leave.one.out=TRUE) or N columns otherwise. Basically, the observations are repeated N times as rows of a matrix.
}
\examples{
  # Example:
  obs <- rnorm(10)
  ClimEns(obs)
}



