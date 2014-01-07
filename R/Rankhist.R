#########################################
#                                       # 
# RANK HISTOGRAM FOR ENSEMBLE FORECASTS #
#                                       #
#########################################
Rankhist <- function(ens, ver) {
#
# calculate the rank histogram of a collection of ensemble forecasts
#
# Usage: 
#   rh <- rankhist(ens=ens, ver=ver)
#
# Arguments:
#
#   ens ... N*K matrix, rows are the ensemble forecasts
#   ver ... N vector of corresponding verifications
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
#   ver <- rnorm(100)
#   rh <- rankhist(ens, ver)
#
# References: 
#   Talagrand (1997)
#   Hammill (2001) http://dx.doi.org/10.1175/1520-0493(2001)129%3C0550:IORHFV%3E2.0.CO;2
#
#
  N <- dim(ens)[1]
  K <- dim(ens)[2]
  stopifnot(N == length(ver))
  ranks <- apply(cbind(ver, ens), 1, rank, ties.method="random")[1, ]
  rank.hist <- hist(ranks, breaks=seq(0.5, K+1.5, 1), plot=FALSE)$counts
  return(rank.hist)
}

