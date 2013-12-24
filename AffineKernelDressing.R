FitAkdParameters <- function(ens, obs) {

  #       p(y|x) = 1 / K * sum {dnorm(y, z.i(x), s(x))}
  # where   s(x) = s1 + s2 * (4 / 3 / K) ^ 0.4 * var(x)
  # and   z.i(x) = r0 + r1 * mean(x) + r2 * x[i]

  # ensemble means
  m.x <- rowMeans(ens, na.rm=TRUE)
  # ensemble variance scaled by silverman factor ^ 2
  k <- rowSums(!is.na(ens))
  sf.2 <- (4 / 3 / k) ^ 0.4
  v.x <-  sf.2 * apply(ens, 1, var, na.rm=TRUE)

  # initial guesses
  m1 <- lm(obs~m.x)
  m2 <- lm(resid(m1)~v.x)
  r0 <- coef(m1)[1]
  s1 <- coef(m2)[1]
  s2 <- 1
<<<<<<< HEAD
  r2 <- ifelse(test = (coef(m2)[2] <= s2),
=======
  r2 <- ifelse(test = coef(m2)[2] <= s2,
>>>>>>> 02cb66086e8c0ddfcf731a27f5658cf41ee39b75
               yes = 0,
               no = sqrt((coef(m2)[2] - s2) * sf.2))
  r1 <- coef(m1)[2] - r2

  # optimizing the dressing crps using optim ?
  parms <- c(r0, r1, r2, s1, s2)
  crps <- function(parms) {
<<<<<<< HEAD
    # ...
  }
  # optim ...
=======
    
  }
>>>>>>> 02cb66086e8c0ddfcf731a27f5658cf41ee39b75

  


}

