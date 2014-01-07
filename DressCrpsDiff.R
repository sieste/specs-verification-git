################################
#
# ANALYZE DIFFERENCE IN THE CRPS BETWEEN TWO DRESSED ENSEMBLES
# FOR THE SAME VERIFICATION
#
# ens     ... dressed ensemble (object of class `dressed.ens`)
# ens.ref ... dressed reference ensemble (object of class `dressed.ens`)
# ver     ... verifications (vector of length N)
# probs   ... quantiles of the sampling distribution
#
################################
DressCrpsDiff <- function(dressed.ens, dressed.ens.ref, ver, probs=NA) {

  # sanity checks
  stopifnot(class(dressed.ens)=="dressed.ens", 
            class(dressed.ens.ref)=="dressed.ens")
  stopifnot(is.vector(ver), length(ver) > 1)

  ens <- dressed.ens[["ens"]]
  ens.ref <- dressed.ens.ref[["ens"]]

  stopifnot(nrow(ens)==length(ver), nrow(ens.ref) == length(ver))

  N <- length(ver)
  K <- ncol(ens)
  K.ref <- ncol(ens.ref)
  ver <- matrix(ver, ncol=1)

  K <- ncol(ens)
  K.ref <- ncol(ens.ref)

  # calculate crps difference
  crps.ens <- DressCrps(dressed.ens, ver)
  crps.ref <- DressCrps(dressed.ens.ref, ver)
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

  #return
  list(crps.diff=mean.crps.diff, sampling.quantiles=cis)
}

