################################
#
# THE FAIR CONTINUOUSLY RANKED PROBABILITY SCORE
#
# ens ... ensemble values (matrix of dimension N*K)
# ver ... verification (vector of length N)
#
# references: * Gneiting, Raftery (2007) "Probabilistic forecasts,
#               calibration and sharpness"
#             * Ferro, Richardson, Weigel (2008) "On the effect 
#               of ensemble size on the discrete and continuous
#               ranked probability scores"
#
################################
FairCrps <- function(ens, ver) {
  ver <- as.vector(ver)
  if (length(ver) == 1) {
  # single instance
    if (is.matrix(ens)) {
      ens <- as.vector(ens)
    }
    K <- length(ens)
    if (K == 1) {
    # for one ensemble member, the crps reduces to the absolute error
      crps <- abs(ens - ver)
    } else {
      crps <- mean(abs(ens - ver)) - sum(dist(ens)) / K / (K - 1)
    }
  } else {
  # multiple instances
    N <- length(ver)
    K <- ncol(ens)
    stopifnot(length(ver) == nrow(ens))
    if (K == 1) {
      crps <- abs(ens - ver)
    } else {
      crps <- sapply(1:N, function(i) 
                mean(abs(ens[i,] - ver[i])) - sum(dist(ens[i,])) / K / (K - 1)
              )
    }
  }
  return(crps)
}

