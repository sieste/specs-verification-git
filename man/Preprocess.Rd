\name{Preprocess}
\alias{Preprocess}
\title{Preprocess data for use with SpecsVerification.}
\description{
Preprocess ensemble and observation data for use with verification routines.
}
\usage{
Preprocess(ens=NULL, ens.ref=NULL, obs=NULL)
}
\arguments{
  \item{ens}{A data.frame, vector, or matrix; or NULL (default).}
  \item{ens.ref}{A data.frame, vector, or matrix; or NULL (default).}
  \item{obs}{A data.frame, vector, or matrix; or NULL (default).}
}
\value{
  The function returns a list with elements ens, ens.ref and obs.
}
\details{
  Here is roughly what the function does, in that order:

  - check which of the three arguments were provided by comparing them to NULL\cr
  - check if ens and ens.ref and obs are of class `data.frame`, `numeric`, or `matrix`; otherwise stop\cr
  - vector-valued ensembles are interpreted as one K-member ensemble, rather than K 1-member ensembles\cr
  - ens and ens.ref are coerced to class `matrix` by `as.matrix`, obs to class `numeric` i.e. vector\cr
  - check whether all provided objects have the same length in the time dimension; otherwise stop\cr
  - ensemble members (i.e. columns) which have all entries NA are removed\cr
}
\examples{
  # Example:
  ens <- matrix(1, nrow=2, ncol=3)
  ens.ref <- data.frame(c(1,2), c(3,NA), c(NA,NA))
  obs <- c(5,NA)
  Preprocess(ens=ens, ens.ref=ens.ref, obs=obs)
}




