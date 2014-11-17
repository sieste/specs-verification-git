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

  # pre-process
  l <- Preprocess(ens=ens, ens.ref=ens.ref, obs=obs)
  ens <- l[["ens"]]
  ens.ref <- l[["ens.ref"]]
  obs <- l[["obs"]]

  # sanity checks
  stopifnot(length(tau) == 1 | length(tau) == length(obs))

  N <- length(obs)

  # calculate Brier skill score 
  br.ens <- EnsBrier(ens, obs, tau)
  br.ref <- EnsBrier(ens.ref, obs, tau)
  bss <- 1 - mean(br.ens) / mean(br.ref)

  # update N
  N <- N - sum(is.na(br.ens+br.ref))

  # calculate error propagation standard deviation
  bss.sigma <- ifelse(N > 1,
         1 / sqrt(N) * sqrt( var(br.ens) / mean(br.ref)^2 + 
         var(br.ref) * mean(br.ens)^2 / mean(br.ref)^4 - 
         2 * cov(br.ens, br.ref) * mean(br.ens) / mean(br.ref)^3),
         NA)

  #return
  list(bss=bss, bss.sigma=bss.sigma)
}

