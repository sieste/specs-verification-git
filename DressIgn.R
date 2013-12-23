# return a vector of Ignorance scores for dressed ensembles
DressIgn <- function(dressed.ens, obs) {
  N <- nrow(dressed.ens[["ens"]])

  ign <- with(dressed.ens, {
    sapply(1:N, function(ii) {
      s <- ker.wd[ii, ]
      e <- ens[ii, ]
      o <- obs[ii]
      -log2(mean(dnorm(o, e, s), na.rm=TRUE))
    })
  })

  # return
  ign
}


