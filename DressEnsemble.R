DressEnsemble <- function(ens, dressing.method="silverman", parameters=NA) {

  if (dressing.method == "silverman") {
    # silverman's rule of thumb
    n.members <- rowSums(1 - is.na(ens))
    if (any(n.members==1)) {
      warning("Some ensembles have only one member. The kernel width is set to zero for these.")
    }
    stdevs <- apply(ens, 1, sd, na.rm=TRUE)
    ker.wd <- (4 * stdevs^5 / (3 * n.members))^0.2

    K <- max(n.members)
    ker.wd <- matrix(rep(ker.wd, K), ncol=K)

    ker.type <- "gauss"
  }

  # create object
  dressed.ens <- list(ens=ens, ker.wd=ker.wd, ker.type=ker.type)
  class(dressed.ens) <- "dressed.ens"

  # return
  dressed.ens
}

# return ensemble density at a matrix of x values
GetDensity <- function(dressed.ens, x) {

  if (dressed.ens[["ker.type"]] == "gauss") {
    kernel.fun <- dnorm
  } else {
    stop("unknown kernel type")
  }
  stopifnot(is.matrix(x))
  
  N <- nrow(dressed.ens[["ens"]])
  if (nrow(x) == 1) {
    x <- matrix(rep(x, N), nrow=N, byrow=TRUE)
  }
  stopifnot(nrow(x) == N)

  d <-  
  sapply(1:ncol(x), function(j) {
    sapply(1:nrow(x), function(i) {
      with(dressed.ens, 
        mean(kernel.fun(x=x[i,j], mean=ens[i, ], sd=ker.wd[i, ]), na.rm=TRUE)
      )
    })
  })

  # return
  d
}

