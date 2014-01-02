DressEnsemble <- function(ens, dressing.method="silverman", 
                          parameters=NA) 
{

  # silverman's rule of thumb
  if (dressing.method == "silverman") {
    n.members <- rowSums(!is.na(ens))
    if (any(n.members==1)) {
      warning(paste("Some ensembles have only one member.",
                    "The kernel width is set to zero for these."))
    }
    stdevs <- apply(ens, 1, sd, na.rm=TRUE)
    ker.wd <- (4 * stdevs^5 / (3 * n.members))^0.2

    K <- max(n.members)
    ker.wd <- matrix(rep(ker.wd, K), ncol=K)

    ker.type <- "gauss"
  }

  # affine kernel dressing
  #
  #       p(y|x) = 1 / K * sum {dnorm(y, z.i(x), s(x))}
  # where   s(x) = s1 + s2 * (4 / 3 / K) ^ 0.4 * var(x)
  # and   z.i(x) = r0 + r1 * mean(x) + r2 * x[i]
  if (dressing.method=="akd") {
    stopifnot(all(names(parameters) %in% c("r0", "r1", "r2", "s1", "s2")))
    k <- rowSums(!is.na(ens))
    var.ens <- apply(ens, 1, var, na.rm=TRUE)
    z <- with(parameters, r0 + r1 * rowMeans(ens, na.rm=TRUE) + r2 * ens)
    s2 <- with(parameters, s1 + s2 * (4 / 3 / k) ^ .4 * var.ens)
    s <- sapply(s2, function(x) sqrt(max(x, 0)))
 
    K <- max(k)
    ker.wd <- matrix(rep(s, K), ncol=K)
    ens <- z
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

