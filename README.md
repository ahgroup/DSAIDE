[![Travis-CI build Status](https://travis-ci.org/ahgroup/DSAIDE.svg?branch=master)](https://travis-ci.org/ahgroup/DSAIDE)

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ahgroup/DSAIDE?branch=master&svg=true)](https://ci.appveyor.com/project/ahgroup/DSAIDE)

[![Coverage status](https://codecov.io/gh/ahgroup/DSAIDE/branch/master/graph/badge.svg)](https://codecov.io/github/ahgroup/DSAIDE?branch=master)

# DSAIDE
Dynamical Systems Approach to Infectious Disease Epidemiology

## Description
This is an R package consisting of a set of Shiny Apps to teach infectious disease epidemiology from a dynamical system perspective.
By manipulating the models through the Shiny UI interface and working through the instructions provided within the Shiny UI, you can learn about some important concepts in infectious disease epidemiology. 
You will also learn how models can be used to study such concepts.

## Installing
1. Install the CRAN release in the usual way with `install.packages('DSAIDE')`.
2. The latest development version (potentially buggy) can be installed from github, using the devtools package. If you don't have it, install the devtools package. Then, install DSAIDE through devtools. The following commands should get you up and running:

```r
install.packages('devtools')
library('devtools')
install_github('ahgroup/DSAIDE', build_vignettes = TRUE)
```

The extra `build_vignettes` ensures you get the vignette (tutorial) for the package installed/created.


## Basic Use
After install (which you need to do only once), load the package by runing `library('DSAIDE')`. You should receive a short greeting. Now you can open the DSAIDE main menu by running `dsaidemenu()`. From the main menu, choose the different apps corresponding to different modeling topics and scenarios. Each app contains a description of the model and scenario that is implemented. Each app also contains a list of recommeded tasks to work through in order to learn about a specific topic. Once done exploring the apps, close the main menu to exit back to the R console.

## Alternative Use
If you don't want to use the main menu, there is another way to run each app. Run the function `dsaideapps()` to get a list of all available apps. Run the same function specifying an app (with quotation marks), e.g. `dsaideapps('IDControl')` to run that specific app. Once you close the app, you'll be back at the `R` console, then use the same function to run a different app. 

## Advanced Use
You can call the underlying simulation functions directly from the `R` console. You can also access all functions and modify them to your own needs. To find the folder on your computer where the simulator functions are stored, use the following command:

```r
system.file("simulatorfunctions", package = "DSAIDE")
```
See [the package vignette](https://rawgit.com/ahgroup/DSAIDE/master/inst/doc/DSAIDE.html) for more details on the different ways to use the package. You can get to the vignette by typing `vignette('DSAIDE')` at the `R` console. Also see [the DSAIDE publication in PLoS Comp Bio](https://doi.org/10.1371/journal.pcbi.1005642).



## Contributing to the package
DSAIDE is built in a way that makes it (hopefully) easy for others to contribute new simulations/apps. The package contains a sub-folder called _/docsfordevelopers_ (in the locally installed version of the package, this folder is in the main package folder, on Github it is inside the _/inst_ folder). The documentation.md file in this folder explains the overall structure of the package and gives detailed instructions on how to create new apps. The information provided is meant to be detailed and complete enough to allow fairly easy development and contribution of new apps (or other enhancements) to the package. For any further questions, feel free to contact me via email or github.

## Further information
* I published a paper describing the package and how to use it which you can find and read [here](https://doi.org/10.1371/journal.pcbi.1005642). Also use this paper if you want to cite the package.
* The vignette provides details about the different ways the package can be used. Currently the paper is probably better than the vignette, but in the future the vignette will be more up-to-date. 
* The `documentation.md' file described above contains more information about the package structure.
* For feedback, bug reports etc., file a github issue.
* A 'companion' package to this one, called Dynamical Systems Approaches for Immune Respone Modeling (DSAIRM), focuses on models for analyzing with-host models. It has the same structure as DSAIDE. [See the DSAIRM Github site for more information.](https://github.com/ahgroup/DSAIRM)
 

## Contributors
This R package is developed and maintained by [Andreas Handel](http://handelgroup.publichealth.uga.edu/). The following individuals have made contributions to this package: Isaac Fung, Spencer Hall, Ben Listyg, Brian McKay, John Rossow, Sina Solaimanpour, Henok Woldu.
