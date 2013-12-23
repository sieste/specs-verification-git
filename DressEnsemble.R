DressEnsemble <- function(ens, dressing.method="silverman", parameters=NA) {

  if (dressing.method == "silverman") {
    # silverman's rule of thumb
    k <- rowSums(1 - is.na(ens))
    stdevs <- apply(ens, 1, sd, na.rm=TRUE)
    ker.wd <- (4 * stdevs^5 / (3 * k))^0.2
  }

  # create object
  dressed.ens <- list(ens=ens, ker.wd=ker.wd)
  class(dressed.ens) <- "dressed.ens"

  # return
  dressed.ens
}


