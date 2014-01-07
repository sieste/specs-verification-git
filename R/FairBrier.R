################################
#
# THE FAIR BRIER SCORE
#
# ens ... ensemble matrix (N*K)
# ver ... verification vector (N)
# tau ... exceedance threshold that defines the event
#
# reference: Ferro (2013) "Fair scores for ensemble forecasts"
#
################################
FairBrier <- function(ens, ver, tau=0.5) {
  ver <- matrix(ver, ncol=1)
  if (is.null(dim(ens))) {
    ens <- matrix(ens, nrow=1)
  }
  stopifnot(nrow(ens) == nrow(ver))

  K <- ncol(ens)
  i <- rowSums(ens > tau)
  j <- 1 * (ver > tau)
  fb <- (j - i / K) ^ 2 - i * (K - i) / K / K / (K - 1)
  return(fb)
}

