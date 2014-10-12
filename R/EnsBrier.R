################################
#
# THE BRIER SCORE FOR ENSEMBLE FORECASTS
#
# ens ... ensemble matrix (N*K)
# obs ... observation vector (N)
# tau ... exceedance threshold that defines the event
#
################################
EnsBrier <- function(ens, obs, tau=0.5) {
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
  br <- (j - i / K) ^ 2 
  return(br)
}

