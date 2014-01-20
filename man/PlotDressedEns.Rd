\name{PlotDressedEns}
\alias{PlotDressedEns}
\title{Plot a series forecast distributions of dressed ensembles}
\description{
  The forecast distribution of a series of dressed ensemble forecasts is calculated and plotted as polygons over time. A number of options is available to add diagnostic information to the plot. 
}
\usage{
PlotDressedEns(dressed.ens, add=FALSE, obs=NULL, plot.ens=FALSE, plot.ker=FALSE)
}
\arguments{
  \item{dressed.ens}{An object of class `dressed.ens`. See ?DressEnsemble for details.}
  \item{add}{boolean, default=FALSE. If TRUE, no new plotting device is created and everything is added to an existing device.}
  \item{obs}{A vector of length N, default=NULL. The verifying observations corresponding to the individual ensemble forecasts. If a vector of length N is provided (N = nrow(dressed.ens[["ens"]]), the values are added to the plot as markers.}
  \item{plot.ens}{boolean, default=FALSE. If TRUE, the centers of the individual dressing kernels are indicated by markers.}
  \item{plot.ker}{boolean, default=FALSE. If TRUE, the individual dressing kernels are plotted.}
}
\value{
none
}
\examples{
  # Example:
  ens <- matrix(rnorm(20),4,5)
  obs <- rnorm(4)
  d.ens <- DressEnsemble(ens)
  PlotDressedEns(d.ens, add=FALSE, obs=obs, plot.ens=FALSE, plot.ker=TRUE)
}


