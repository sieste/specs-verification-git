################################
#
# ANALYZE DIFFERENCE IN THE FAIR BRIER SCORE BETWEEN TWO ENSEMBLE
# FORECASTING SYSTEMS FOR THE SAME VERIFICATION
#
# ens     ... ensemble to be tested (matrix of dimension N*K)
# ens.ref ... reference forecast ensemble (matrix of dimension N*K.ref)
# ver     ... verifications (vector of length N)
# tau     ... threshold, whose exceedance defines the "event" (scalar, or
#             vector of length N)
#             the default is 0.5, such that ensemble members can be given as
#             event indicators, i.e. 0 or 1
#
# return value: a list with elements
#     * br.diff ... the fair Brier Score difference
#     * sampling.quantiles ... if `probs` were defined, the corresponding
#                              quantiles of the sampling distribution 
#                              of `br.diff`
#
################################
FairBrierDiff <- function(ens, ens.ref, ver, tau=0.5, probs=NA) {

  # sanity checks
  stopifnot(is.numeric(c(ens, ens.ref, ver, tau)))
  stopifnot(is.vector(ver))
  stopifnot(length(ver) > 1)
  stopifnot(is.matrix(ens))
  stopifnot(is.matrix(ens.ref))
  stopifnot(nrow(ens)==length(ver))
  stopifnot(nrow(ens.ref) == length(ver))
  stopifnot(is.numeric(tau))
  stopifnot(length(tau) == 1 | length(tau) == length(ver))

  N <- length(ver)
  K <- ncol(ens)
  K.ref <- ncol(ens.ref)

  # calculate fair Brier score differences
  br.ens <- fairbrier(ens, ver, tau)
  br.ref <- fairbrier(ens.ref, ver, tau)
  br.diff <- br.ref - br.ens
  mean.br.diff <- mean(br.diff)

  # quantiles of the sampling distribution 
  cis <- NA
  if (!any(is.na(probs))) {
    stopifnot(all(probs > 0 & probs < 1))
    probs <- sort(probs)
    cis <- qt(probs, df=N-1) * sd(br.diff) / sqrt(N) + mean.br.diff
    names(cis) <- paste(probs)
  }

  #return
  list(br.diff=mean.br.diff, sampling.quantiles=cis)
}

