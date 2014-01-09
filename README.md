## SPECS verification

This is the official repository of forecast verification routines for the SPECS FP7 project.

### Download, build, install, load

Download the development branch of this repository to `/tmp`

    cd /tmp
    wget "https://github.com/sieste/specs-verification-git/archive/develop.zip" 
    unzip develop.zip

Build the package:

    R CMD build specs-verification-git-develop
    
Install the package:

    R CMD INSTALL SpecsVerification_{%Version}.tar.gz -l /tmp

Load the package in an R session and list all its functions:

    library("SpecsVerification", lib.loc="/tmp")
    ls("package:SpecsVerification")


### Changelog

* 2014-01-09: added and documented `PlotDressedEns` function
* 2014-01-08: Version 0.0-1 ready for testing
