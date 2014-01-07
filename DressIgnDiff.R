################################
#
# ANALYZE DIFFERENCE IN THE IGNORANCE BETWEEN TWO DRESSED ENSEMBLES
# FOR THE SAME VERIFICATION
#
# ens     ... dressed ensemble (object of class `dressed.ens`)
# ens.ref ... dressed reference ensemble (object of class `dressed.ens`)
# ver     ... verifications (vector of length N)
# probs   ... quantiles of the sampling distribution
#
################################
DressIgnDiff <- function(dressed.ens, dressed.ens.ref, ver, probs=NA) {

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
  ign.ens <- DressIgn(dressed.ens, ver)
  ign.ref <- DressIgn(dressed.ens.ref, ver)
  ign.diff <- ign.ref - ign.ens
  mean.ign.diff <- mean(ign.diff)

  # quantiles of the sampling distribution 
  cis <- NA
  if (!any(is.na(probs))) {
    stopifnot(all(probs > 0 & probs < 1))
    probs <- sort(probs)
    cis <- qt(probs, df=N-1) * sd(ign.diff) / sqrt(N) + mean.ign.diff
    names(cis) <- paste(probs)
  }

  #return
  list(ign.diff=mean.ign.diff, sampling.quantiles=cis)
}

