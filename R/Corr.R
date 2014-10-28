################################
#
# THE CORRELATION COEFFICIENT OF THE ENSEMBLE MEAN
#
# ens ... ensemble values (matrix of dimension N*K)
# obs ... observations (vector of length N)
#
################################
Corr <- function(ens, obs, probs=c(0.025, 0.975)) {

  if (class(ens) == "data.frame") {
    ens <- as.matrix(ens)
  }
  if (class(obs) == "data.frame") {
    obs <- as.matrix(obs)
  }
  obs <- as.vector(obs)
  N <- length(obs)
  if (N == 1) {
    stop("Cannot calculate correlation for a single instance")
  }

  ens.mean <- rowMeans(ens)
  stopifnot(length(obs) == length(ens.mean))

  cc <- cor(obs, ens.mean)

  # calculate confidence interval by Fisher z-transform, return NA if N < 4
  if (N < 4) {
    ci <- NA * probs
  } else {
    ci <- tanh(atanh(cc) + qnorm(probs)/sqrt(N-3))
  }
  names.ci <- paste("q",round(probs, 4), sep="")

  # calculate p value by one-sided t-test (vonstorch & zwiers, p149)
  if (N < 3) {
    p.val <- NA
  } else {
    t <- sqrt((N-2) * cc * cc / (1 - cc * cc))
    p.val <- 1-pt(t, df=N-2)
  }
  ret <- c(cc, ci, p.val)
  names(ret) <- c("corr", names.ci, "p.value")

  return(ret)
}

