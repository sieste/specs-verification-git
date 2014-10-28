################################
#
# ANALYZE DIFFERENCE IN THE RANKED PROBABILITY SCORE BETWEEN TWO ENSEMBLE
# FORECASTING SYSTEMS FOR THE SAME OBSERVATION
#
# ens     ... ensemble to be tested, ens[i,j] is the number of ensemble members that predict category j at time i
# ens.ref ... reference forecast ensemble, same dimension as ens
# obs     ... observations matrix, same dimension as ens, obs[i,j] = 1 if category j occured at time i, 0 otherwise
#
# return value: a list with elements
#     * rps.diff ... the Ranked Probability Score difference
#     * sampling.quantiles ... if `probs` were defined, the corresponding
#                              quantiles of the sampling distribution 
#                              of `rps.diff`
#
################################
EnsRpsDiff <- function(ens, ens.ref, obs, probs=NA) {

  # sanity checks
  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(ens.ref) == "data.frame") {
    ens.ref <- as.matrix(ens.ref)
  }
  if (class(obs) == "data.frame") {
    obs <- as.matrix(obs)
  }
  stopifnot(is.numeric(c(ens, ens.ref, obs)))
  stopifnot(is.matrix(obs), is.matrix(ens), is.matrix(ens.ref))
  stopifnot(all(dim(ens) == dim(obs)))
  stopifnot(all(dim(ens.ref) == dim(obs)))
  stopifnot(all.equal(ncol(ens), ncol(ens.ref), ncol(obs)))

  N <- nrow(obs)

  # calculate RPS score differences
  rps.ens <- EnsRps(ens, obs)
  rps.ref <- EnsRps(ens.ref, obs)
  rps.diff <- rps.ref - rps.ens
  mean.rps.diff <- mean(rps.diff)

  # quantiles of the sampling distribution 
  cis <- NA
  if (!any(is.na(probs))) {
    stopifnot(all(probs > 0 & probs < 1))
    probs <- sort(probs)
    cis <- qt(probs, df=N-1) * sd(rps.diff) / sqrt(N) + mean.rps.diff
    names(cis) <- paste(probs)
  }

  # p value of paired one-sided t test for positive score difference
  p.value <- 1-pt(mean.rps.diff / sd(rps.diff) * sqrt(N), df=N-1)

  #return
  list(rps.diff=mean.rps.diff, sampling.quantiles=cis, p.value=p.value)
}

