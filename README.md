## SPECS verification

This is the official repository of forecast verification routines for the SPECS FP7 project.

### Download, install, load

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
