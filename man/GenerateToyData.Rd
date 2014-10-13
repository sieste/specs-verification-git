\name{GenerateToyData}
\alias{GenerateToyData}
\title{Generate toy data for ensemble verification.}
\description{
Generate synthetic ensemble and observation data, using a latent variable model. See Details.
}
\usage{
GenerateToyData(N, mu.y, s.s, s.eps, mu.x1, beta1, s.eta1, K1, 
                mu.x2, beta2, s.eta2, K2) 
}
\arguments{
  \item{N}{number of forecasts and observations}
  \item{mu.y}{expectation value of the observations}
  \item{s.s}{standard deviation of the predictable signal}
  \item{s.eps}{standard deviation of the unpredictable noise}
  \item{mu.x1}{expectation value of the ensemble}
  \item{beta1}{weighting parameter of the signal in the ensemble forecasting system}
  \item{s.eta1}{average spread of the ensemble}
  \item{K1}{number of members of the ensemble}
  \item{mu.x2}{expectation value of the reference ensemble}
  \item{beta2}{weighting parameter of the signal in the reference ensemble forecasting system}
  \item{s.eta2}{average spread of the reference ensemble}
  \item{K2}{number of members of the reference ensemble}
}
\value{
  A list with elements:

  "obs": N-vector of observations

  "ens": N*K1 matrix of ensemble members

  "ens.ref": N*K2 matrix of reference ensemble members
}
\details{
  The function simulates data from the latent variable model:

    y_t   = mu_y +     s_t    + eps_t

    x_t,r = mu_x + beta * s_t + eta_t,r

where y_t is the observation at time t, and x_t,r is the r-th ensemble member at time t. The latent variable s_t is to be understood as the "predictable signal" that is generates correlation between observations and ensemble members. 
}
\examples{
  l <- GenerateToyData()
  with(l, EnsCrps(ens, obs))
}

