## SPECS verification

This is the official repository of forecast verification routines for the SPECS FP7 project.

### Now available on CRAN

`SpecsVerification` is available on the Comprehensive R Archive Network ([CRAN](http://cran.R-project.org)). This means it can be installed from R by

    install.packages("SpecsVerification")

It is still under development and will see some changes in the future, but the main bugs have been fixed. If you want to get emails if new versions of `SpecsVerification` become available, write an email to `s.siegert (at) exeter (dot) ac (dot) uk`.
    

### Manual download, install, load

These instructions show you how to install the latest version of the `SpecsVerification` package from github. Modifications will first appear on this repository before they are submitted to CRAN.

**Download** the master branch of this repository to `/tmp`. In the Linux shell, do

    cd /tmp
    wget "https://github.com/sieste/specs-verification-git/archive/master.zip"
    unzip master.zip

Mac and Windows users click on the `Download ZIP` link on the right of this page and save to a temporary directory. Extract the zip archive to the temporary directory using your favourite file extraction tool.

**Install** the package from source. Open a `R` session and enter the command

    install.packages("/tmp/specs-verification-git-master", repo=NULL, lib="/tmp", type="source")
    
Replace the `/tmp` directory as necessary. Omitting the `lib="/tmp"` will install to your default library directory, which you probably do not want at the present stage of development. In order to install the current development branch, simply replace `master` by `develop` in all the above commands.

To **load** the package from the library directory `/tmp`, and list all its functions, do

    library("SpecsVerification", lib.loc="/tmp")
    ls("package:SpecsVerification")


### Changelog

* 2015-10-23: **version 0.4-1** available on CRAN
* 2015-10-23: fixed bug in `Detrend` (thanks Matteo!)
* 2015-10-23: added option `attributes` to `ReliabilityDiagram` to plot the no-skill and no-resolution lines (thanks Igor!)
* 2015-09-11: **version 0.4-0** available on CRAN
* 2015-06-03: added functions to preprocess ensemble and observation data (e.g. transform from data.frame to matrix, handle NA's, etc)
* 2014-10-28: implemented and documented `Corr` and `CorrDiff` routines, that calculate correlation of the ensemble mean, and difference between two correlation coefficients evaluated over the same observation, including confidence intervals and p values 
* 2014-10-15: added `reduce.bins` option to `Rankhist` function
* 2014-10-13: implemented and documented `Detrend` function
* 2014-10-13: implemented and documented `GenerateToyData` function
* 2014-10-12: implemented and documented functions `EnsCrps`, `EnsRps`, and `EnsBrier` (the "unfair" scores), and corresponding score difference, and skill score functions
* 2014-08-27: added p-value of paired one-sided t-test as a return value to `ScoreDiff` functions
* 2014-08-25: added bootstrapping option to `GaussCrpsDecomposition` and `BrierScoreDecomposition` functions
* 2014-08-19: implemented and documented `ClimEns` function to construct climatological ensembles from a vector of observations
* 2014-08-19: implemented and documented CRPSS and Ignorance skill score for dressed ensembles, Fair Brier Skill score, Fair CRPSS, Fair RPSS, and Gaussian CRPSS
* 2014-05-02: **Version 0.1-1** available on CRAN
* 2014-04-30: specified which score difference are calculated in the documentation of the `*Diff` functions.
* 2014-04-30: added and documented functions `GaussCRPS`, `GaussCrpsDiff` and `GaussCrpsDecomposition`: Continuously ranked probability score for probability forecasts issued as Gaussian distributions
* 2014-04-24: adjusted all functions to handle `data.frame`s (thanks, David!)
* 2014-04-11: fixed a bug in the `FitAkdParameters` routine (thanks, David!)
* 2014-04-09: added and documented `FairRps` and `FairRpsDiff`: Fair ranked probability score for categorical ensemble forecasts
* 2014-02-07: added `integrated` option to `GetDensity()` function
* 2014-01-27: **Version 0.0-2** ready for testing
* 2014-01-27: added user guide
* 2014-01-20: added lots of documentation, fixed optional dependency on `multicore`, renamed `RankhistTests` to `TestRankhist`
* 2014-01-19: major cleanup
* 2014-01-19: `FairBrier`: added documentation
* 2014-01-19: `BrierScoreDecomposition`: added documentation, fixed some bugs
* 2014-01-14: `PlotRankhist`: added documentation
* 2014-01-13: removed GSL dependency 
* 2014-01-09: added and documented `GetDensity` function for dressed ensembles
* 2014-01-09: added and documented `PlotDressedEns` function
* 2014-01-08: **Version 0.0-1** ready for testing
