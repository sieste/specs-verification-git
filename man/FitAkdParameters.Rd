\name{FitAkdParameters}
\alias{FitAkdParameters}
\title{Fit parameters for affine kernel dressing}
\description{
  The 5 parameters of affine kernel dressing are fitted by minimum CRPS estimation.
}
\usage{
FitAkdParameters(ens, obs)
}
\arguments{
  \item{ens}{matrix, N rows, K columns. An archive of K-member ensemble forecasts for N time instances.}
  \item{obs}{vector of length N. The verifying observations corresponding to the N ensemble forecasts.}
}
\value{
The function returns a list of parameters for affine kernel dressing. 
}
\details{
Affine Kernel Dressing transforms the discrete K-member forecast ensemble at time instance n, `ens[n, ]`, to a continuous distribution function for the target `y` by the following equation:

      p(y|ens) = 1 / K * sum {dnorm(y, z.i, s)} \cr
where   s = (4/3/K)^0.4 * (s1 + s2 * a^2 * var(ens)) \cr
and   z.i = r1 + r2 * mean(ens) + a * ens

The parameters r1, r2, a, s1, s2 are fitted by minimizing the continuously ranked probability score (CRPS) of the ensemble forecasts `ens` with respect to the observations `obs`. The optimization is carried out using the R function `optim(...)`.

The dressing variance is a linear function of the ensemble variance, and can thus become negative for certain parameter settings. In order to ensure nonnegativity, a logarithmic barrier is introduced during the optimization. That is, the method does not minimize the mean CRPS, but the function:

        mean(CRPS(ens, obs)) * (1 + 0.01 * barrier) \cr
where   barrier = max(0, -log( minimal dressing variance )) 

Since the evaluation of the CRPS is numerically expensive, the optimization can take a long time. Speed can be increased by optimizing the parameters only for a part of the forecast instances.
}
\examples{
  # Example:
  ens <- matrix(rnorm(500),100,5)
  obs <- rnorm(100)
  FitAkdParameters(ens, obs)
}
\references{
Broecker J. and Smith L. (2008). From ensemble forecasts to predictive distribution functions. Tellus (2008), 60A, 663--678. DOI 10.1111/j.1600-0870.2008.00333.x.
}


