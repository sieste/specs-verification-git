###################################
#
# CREATE A CLIMATOLOGICAL ENSEMBLE FROM A VECTOR OF OBSERVATIONS
#
# INPUT: OBS ... A VECTOR OF LENGTH N
# OUTPUT: ENS.CLIM ... A MATRIX WITH N ROWS AND (N-1) COLUMNS
#
###################################

ClimEns <- function(obs, leave.one.out=TRUE) {

  if (class(obs) == "data.frame") {
    obs <- as.matrix(obs)
  }

  obs <- as.vector(obs)
  obs <- obs[is.finite(obs)]

  if (length(obs) < 2) {
    stop("Need at least 2 valid observations")
  }

  # construct climatological ensemble matrix
  N <- length(obs)
  ens.clim <- t(matrix(rep(obs, N), N, N))
  
  # remove diagonal if desired
  if (leave.one.out) {
    ens.clim <- t(matrix(t(ens.clim)[-seq(1,N^2,N+1)], N-1, N))
  }
  
  return(ens.clim)
}

