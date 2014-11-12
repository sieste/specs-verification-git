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

  # pre-processing
  l <- Preprocess(ens=ens, ens.ref=ens.ref, obs=obs)
  ens <- l[["ens"]]
  ens.ref <- l[["ens.ref"]]
  obs <- l[["obs"]]

  # calculate brier scores
  K <- ncol(ens)
  i <- rowSums(ens > tau)
  j <- 1 * (obs > tau)
  br <- (j - i / K) ^ 2 
  return(br)
}

