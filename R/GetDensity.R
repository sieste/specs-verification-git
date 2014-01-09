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


