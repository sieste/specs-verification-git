################################
#
# ANALYZE DIFFERENCE IN THE BRIER SCORE BETWEEN TWO ENSEMBLE
# FORECASTING SYSTEMS FOR THE SAME OBSERVATION
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
#     * br.diff ... the Brier Score difference
#     * sampling.quantiles ... if `probs` were defined, the corresponding
#                              quantiles of the sampling distribution 
#                              of `br.diff`
#
################################
EnsBrierDiff <- function(ens, ens.ref, obs, tau=0.5, probs=NA) {

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

  # calculate Brier score differences
  br.ens <- EnsBrier(ens, obs, tau)
  br.ref <- EnsBrier(ens.ref, obs, tau)
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

  # p value of paired one-sided t test for positive score difference
  p.value <- 1-pt(mean.br.diff / sd(br.diff) * sqrt(N), df=N-1)

  #return
  list(br.diff=mean.br.diff, sampling.quantiles=cis, p.value=p.value)
}

