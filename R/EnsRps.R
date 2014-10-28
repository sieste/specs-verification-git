################################
#
# THE RANKED PROBABILITY SCORE FOR ENSEMBLE FORECASTS
#
# ens ... ensemble matrix, ens[i,j] = number of ensemble members that predict category j at time i
# obs ... observation matrix, obs[i,j] = 1 if category j is observed at time i, 0 otherwise
#
################################
EnsRps <- function(ens, obs) {

  # sanity checks
  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(obs) == "data.frame") {
    obs <- as.matrix(obs)
  }
  if (is.null(dim(ens))) {
    ens <- matrix(ens, nrow=1)
  }
  if (is.null(dim(obs))) {
    obs <- matrix(obs, nrow=1)
  }
  stopifnot(all(dim(ens)==dim(obs)))
  stopifnot(all(rowSums(obs) == 1))
  stopifnot(all(rowSums(ens) > 1))

  rps <- 
  sapply(1:nrow(ens), function(i) {
    E <- cumsum(ens[i,])
    O <- cumsum(obs[i,])
    M <- tail(E,1)
    sum((O-E/M)^2) 
  })
  return(rps)
}

