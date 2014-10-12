################################
#
# ANALYZE DIFFERENCE IN THE BRIER SCORE BETWEEN TWO ENSEMBLE
# FORECASTING SYSTEMS FOR THE SAME OBSERVATION BY SKILL SCORE 1 - S / S.ref
#
# ens     ... ensemble to be tested (matrix of dimension N*K)
# ens.ref ... reference forecast ensemble (matrix of dimension N*K.ref)
# obs     ... observations (vector of length N)
# tau     ... threshold, whose exceedance defines the "event" (scalar, or
#             vector of length N)
#             the default is 0.5, such that ensemble members can be given as
#             event indicators, i.e. 0 or 1
#
# return value: a list with elements
#     * br.ss ... the Brier Skill Score 
#     * br.sigma ... approximate standard deviation of the skill score
#
################################
EnsBrierSs <- function(ens, ens.ref, obs, tau=0.5) {

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
  stopifnot(is.numeric(c(ens, ens.ref, obs, tau)))
  stopifnot(is.vector(obs))
  stopifnot(length(obs) > 1)
  stopifnot(is.matrix(ens))
  stopifnot(is.matrix(ens.ref))
  stopifnot(nrow(ens)==length(obs))
  stopifnot(nrow(ens.ref) == length(obs))
  stopifnot(is.numeric(tau))
  stopifnot(length(tau) == 1 | length(tau) == length(obs))

  N <- length(obs)
  K <- ncol(ens)
  K.ref <- ncol(ens.ref)

  # calculate Brier skill score 
  br.ens <- EnsBrier(ens, obs, tau)
  br.ref <- EnsBrier(ens.ref, obs, tau)
  bss <- 1 - mean(br.ens) / mean(br.ref)
  bss.sigma <- 1 / sqrt(N) * sqrt( var(br.ens) / mean(br.ref)^2 + 
         var(br.ref) * mean(br.ens)^2 / mean(br.ref)^4 - 
         2 * cov(br.ens, br.ref) * mean(br.ens) / mean(br.ref)^3)

  #return
  list(bss=bss, bss.sigma=bss.sigma)


  #return
  list(bss=bss, bss.sigma=bss.sigma)
}

