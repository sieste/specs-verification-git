################################
#
# ANALYZE DIFFERENCE IN THE CRPS BETWEEN TWO ENSEMBLE
# FORECASTING SYSTEMS FOR THE SAME OBSERVATIONS
#
# ens     ... the ensemble (matrix of dimension N*K)
# ens.ref ... the reference ensemble (matrix of dimension N*K.ref)
# obs     ... observations (vector of length N)
# probs   ... quantiles of the sampling distribution
#
################################
EnsCrpsDiff <- function(ens, ens.ref, obs, probs=NA) {

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
  crps.ens <- EnsCrps(ens, obs)
  crps.ref <- EnsCrps(ens.ref, obs)
  crps.diff <- crps.ref - crps.ens
  mean.crps.diff <- mean(crps.diff)


  # quantiles of the sampling distribution 
  cis <- NA
  if (!any(is.na(probs))) {
    stopifnot(all(probs > 0 & probs < 1))
    probs <- sort(probs)
    cis <- qt(probs, df=N-1) * sd(crps.diff) / sqrt(N) + mean.crps.diff
    names(cis) <- paste(probs)
  }

  # p value of paired one-sided t test for positive score difference
  p.value <- 1-pt(mean.crps.diff / sd(crps.diff) * sqrt(N), df=N-1)

  #return
  list(crps.diff=mean.crps.diff, sampling.quantiles=cis, p.value=p.value)
}

