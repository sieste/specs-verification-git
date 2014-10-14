\name{GenerateToyData}
\alias{GenerateToyData}
\title{Generate toy data for ensemble verification.}
\description{
Generate synthetic ensemble and observation data, using a latent variable model. See Details.
}
\usage{
GenerateToyData(N, mu.y, s.s, s.eps, mu.x, beta, s.eta, K, 
                mu.x.ref, beta.ref, s.eta.ref, K.ref) 
}
\arguments{
  \item{N}{number of forecasts and observations}
  \item{mu.y}{expectation value of the observations}
  \item{s.s}{standard deviation of the predictable signal}
  \item{s.eps}{standard deviation of the unpredictable noise}
  \item{mu.x}{expectation value of the ensemble}
  \item{beta}{weighting parameter of the signal in the ensemble forecasting system}
  \item{s.eta}{average spread of the ensemble}
  \item{K}{number of members of the ensemble}
  \item{mu.x.ref}{expectation value of the reference ensemble}
  \item{beta.ref}{weighting parameter of the signal in the reference ensemble forecasting system}
  \item{s.eta.ref}{average spread of the reference ensemble}
  \item{K.ref}{number of members of the reference ensemble}
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

