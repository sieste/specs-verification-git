################################
#
# THE FAIR BRIER SCORE
#
# ens ... ensemble matrix (N*K)
# obs ... observation vector (N)
# tau ... exceedance threshold that defines the event
#
# reference: Ferro (2013) "Fair scores for ensemble forecasts"
#
################################
FairBrier <- function(ens, obs, tau=0.5) {
  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(obs) == "data.frame") {
    obs <- c(as.matrix(obs))
  }
  obs <- matrix(obs, ncol=1)
  if (is.null(dim(ens))) {
    ens <- matrix(ens, nrow=1)
  }
  stopifnot(nrow(ens) == nrow(obs))

  K <- ncol(ens)
  i <- rowSums(ens > tau)
  j <- 1 * (obs > tau)
  fb <- (j - i / K) ^ 2 - i * (K - i) / K / K / (K - 1)
  return(fb)
}

