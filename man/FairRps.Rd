\name{FairRps}
\alias{FairRps}
\title{Fair Ranked Probability Score for ensemble forecasts.}
\description{
Calculate the Fair Ranked Probability Score for ensemble forecasts and their corresponding observations
}
\usage{
FairRps(ens, obs)
}
\arguments{
  \item{ens}{N*K matrix. ens[i,j] is the number of ensemble members that predict category j at time i.}
  \item{obs}{N*K matrix. obs[i,j] = 1 if category j is observed at time i, 0 otherwise.}
}
\value{
  The function returns a vector of length N with the individual score values of the N ensemble forecasts.
}
\examples{
  # Example:
  ens <- matrix(c(1,2,3, 2,2,2), byrow=TRUE, nrow=2)
  obs <- matrix(c(0,0,1, 0,1,0), byrow=TRUE, nrow=2)
  FairRps(ens,obs)
}
\references{
Ferro, C.A.T. (2013). Fair scores for ensemble forecasts. Q.J.R. Meteorol. Soc. DOI: 10.1002/qj.2270.
}



