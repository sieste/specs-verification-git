

DressEnsemble <- function(ens, obs=NA, dressing.method="silverman") {

  if (dressing.method == "silverman") {
    # silverman's rule of thumb
    dressed.ens <- DressEnsemble.silverman(ens)
  }

  if (dressing.method == "akd") {
    # affine kernel dressing
    dressed.ens <- DressEnsemble.akd(ens, obs)
  }

  # return
  dressed.ens
}


DressEnsemble.silverman <- function(ens) {
# implementation of silverman's rule of thumb
  k <- rowSums(1 - is.na(ens))
  stdevs <- apply(ens, 1, sd, na.rm=TRUE)
  kernel.widths <- (4 * stdevs^5 / (3 * k))^0.2
}

DressEnsemble.wangbishop <- function(ens, obs) {
  NA
}
