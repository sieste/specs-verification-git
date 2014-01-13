## SPECS verification

This is the official repository of forecast verification routines for the SPECS FP7 project.

### Download, build, install, load

**Download** the development branch ([link](https://github.com/sieste/specs-verification-git/archive/master.zip) of this repository to `/tmp`. In the Linux shell, do

    cd /tmp
    wget "https://github.com/sieste/specs-verification-git/archive/develop.zip" 
    unzip develop.zip

Mac and Windows users click on the `Download ZIP` link on the right of this page and save to a temporary directory.

**Build** the package. Open a terminal, go to the directory where you unzipped the above file and do

    R CMD build specs-verification-git-develop
    
**Install** the package by

    R CMD INSTALL SpecsVerification_0.0-1.tar.gz -l /tmp

Open `R`. In `R`, **load** the package, and list all its functions:

    library("SpecsVerification", lib.loc="/tmp")
    ls("package:SpecsVerification")


### Changelog

* 2014-01-09: added and documented `GetDensity` function for dressed ensembles
* 2014-01-09: added and documented `PlotDressedEns` function
* 2014-01-08: Version 0.0-1 ready for testing
