GenerateToyData <- function(N = 20, mu.y = 0, s.s = 7, s.eps = 6, mu.x1 = 0, beta1 = 0.2, s.eta1 = 8, K1 = 10, mu.x2 = NA, beta2 = NA, s.eta2 = NA, K2 = NA) {
  s <- rnorm(N, 0, s.s)
  y <- mu.y + s + rnorm(N, 0, s.eps)
  x1 <- mu.x1 + beta1 * s + matrix(rnorm(N*K1, 0, s.eta1), N, K1)
  if (!any(is.na(c(mu.x2, beta2, s.eta2, K2)))) {
    x2 <- mu.x2 + beta2 * s + matrix(rnorm(N*K2, 0, s.eta2), N, K2)
    ret <- list(obs=y, ens=x1, ens.ref=x2)
  } else {
    ret <- list(obs=y, ens=x1)
  }
  return(ret)
}

