################################
#
# THE CONTINUOUSLY RANKED PROBABILITY SCORE FOR ENSEMBLE FORECASTS
#
# ens ... ensemble values (matrix of dimension N*K)
# obs ... observations (vector of length N)
#
################################
EnsCrps <- function(ens, obs) {

  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(obs) == "data.frame") {
    obs <- as.matrix(obs)
  }

  obs <- as.vector(obs)
  if (length(obs) == 1) {
  # single instance
    if (is.matrix(ens)) {
      ens <- as.vector(ens)
    }
    K <- length(ens)
    if (K == 1) {
    # for one ensemble member, the crps reduces to the absolute error
      crps <- abs(ens - obs)
    } else {
      crps <- mean(abs(ens - obs)) - sum(dist(ens)) / K / K
    }
  } else {
  # multiple instances
    N <- length(obs)
    K <- ncol(ens)
    stopifnot(length(obs) == nrow(ens))
    if (K == 1) {
      crps <- abs(ens - obs)
    } else {
      crps <- sapply(1:N, function(i) 
                mean(abs(ens[i,] - obs[i])) - sum(dist(ens[i,])) / K / K
              )
    }
  }
  return(crps)
}

