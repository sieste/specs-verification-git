\name{EnsRps}
\alias{EnsRps}
\title{Ranked Probability Score for ensemble forecasts.}
\description{
Calculate the Ranked Probability Score for ensemble forecasts and their corresponding observations
}
\usage{
EnsRps(ens, obs)
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
  EnsRps(ens,obs)
}
\references{
Epstein E.S. (1969): A Scoring System for Probability Forecasts of Ranked Categories.  Journal of Applied Meteorology. DOI: 10.1175/1520-0450(1969)008<0985:ASSFPF>2.0.CO;2
}



