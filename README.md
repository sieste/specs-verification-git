# SPECS verification

This is the official repository of forecast verification routines for the SPECS FP7 project.

## Build, install, load

The following builds and installs the package into the `/tmp` directory:

* Clone this repository to `/tmp` 
* Build the package: `R CMD build specs-verification-git -l /tmp`
* Install the package: `R CMD INSTALL /tmp/SpecsVerification_{%Version}.tar.gz -l /tmp`
* Load the package in an R session: `library("SpecsVerification", lib.loc="/tmp")`
