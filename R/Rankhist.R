#########################################
#                                       # 
# RANK HISTOGRAM FOR ENSEMBLE FORECASTS #
#                                       #
#########################################
Rankhist <- function(ens, obs) {
#
# calculate the rank histogram of a collection of ensemble forecasts
#
# Usage: 
#   rh <- rankhist(ens=ens, obs=obs)
#
# Arguments:
#
#   ens ... N*K matrix, rows are the ensemble forecasts
#   obs ... N vector of corresponding observations
#
# Return value:
#   a vector of verification rank frequencies
#
# Author:
#   Stefan Siegert 
#   s.siegert@exeter.ac.uk 
#   December 2013
#
# Example:
#   ens <- matrix(rnorm(5*100), 100, 5)
#   obs <- rnorm(100)
#   rh <- rankhist(ens, obs)
#
# References: 
#   Talagrand (1997)
#   Hammill (2001) http://dx.doi.org/10.1175/1520-0493(2001)129%3C0550:IORHFV%3E2.0.CO;2
#
#

  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(obs) == "data.frame") {
    obs <- c(as.matrix(obs))
  }
  stopifnot(nrow(ens) == length(obs))
  N <- dim(ens)[1]
  K <- dim(ens)[2]
  ranks <- apply(cbind(obs, ens), 1, rank, ties.method="random")[1, ]
  rank.hist <- hist(ranks, breaks=seq(0.5, K+1.5, 1), plot=FALSE)$counts
  return(rank.hist)
}

