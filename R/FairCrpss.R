################################
#
# ANALYZE DIFFERENCE IN THE FAIR CRPS BETWEEN TWO ENSEMBLE
# FORECASTING SYSTEMS FOR THE SAME OBSERVATIONS BY SKILL SCORE
#
# ens     ... the ensemble (matrix of dimension N*K)
# ens.ref ... the reference ensemble (matrix of dimension N*K.ref)
# obs     ... observations (vector of length N)
#
################################
FairCrpss <- function(ens, ens.ref, obs) {

  # sanity checks
  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(ens.ref) == "data.frame") {
    ens.ref <- as.matrix(ens.ref)
  }
  if (class(obs) == "data.frame") {
    obs <- c(as.matrix(obs))
  }
  stopifnot(is.numeric(c(ens, ens.ref, obs)))
  stopifnot(is.vector(obs), length(obs) > 1)
  stopifnot(is.matrix(ens), is.matrix(ens.ref))
  stopifnot(nrow(ens)==length(obs), nrow(ens.ref) == length(obs))

  N <- length(obs)
  K <- ncol(ens)
  K.ref <- ncol(ens.ref)
  obs <- matrix(obs, ncol=1)

  K <- ncol(ens)
  K.ref <- ncol(ens.ref)

  # calculate fair crps difference
  crps.ens <- FairCrps(ens, obs)
  crps.ref <- FairCrps(ens.ref, obs)
  crpss <- 1 - mean(crps.ens) / mean(crps.ref)
  crpss.sigma <- 1 / sqrt(N) * sqrt( var(crps.ens) / mean(crps.ref)^2 + 
         var(crps.ref) * mean(crps.ens)^2 / mean(crps.ref)^4 - 
         2 * cov(crps.ens, crps.ref) * mean(crps.ens) / mean(crps.ref)^3)

  #return
  list(crpss=crpss, crpss.sigma=crpss.sigma)
}

