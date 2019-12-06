[![Travis-CI build Status](https://travis-ci.org/ahgroup/DSAIDE.svg?branch=master)](https://travis-ci.org/ahgroup/DSAIDE)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ahgroup/DSAIDE?branch=master&svg=true)](https://ci.appveyor.com/project/ahgroup/DSAIDE)
[![Coverage status](https://codecov.io/gh/ahgroup/DSAIDE/branch/master/graph/badge.svg)](https://codecov.io/github/ahgroup/DSAIDE?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/DSAIDE)](https://cran.r-project.org/package=DSAIDE)
[![CRAN checks](https://cranchecks.info/badges/summary/DSAIDE)](https://cran.r-project.org/web/checks/check_results_DSAIDE.html)
[![develVersion](https://img.shields.io/badge/devel%20version-0.3.1-blue.svg?style=flat)](https://github.com/ahgroup/DSAIDE)
[![metacran monthly downloads](http://cranlogs.r-pkg.org/badges/DSAIDE)](https://cran.r-project.org/package=DSAIDE)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/DSAIDE?color=ff69b4)](https://cran.r-project.org/package=DSAIDE)



# DSAIDE
Dynamical Systems Approach to Infectious Disease Epidemiology/Ecology/Evolution

## Description
DSAIDE is an R package containing a set of simulation models (apps) that teach infectious disease epidemiology from a dynamical system perspective. 

You can explore the apps through a graphical user interface, implemented in Shiny. By going through the documentation and instructions of each app, you will learn important concepts of infectious disease epidemiology and related areas (e.g. ecology and evolution). You will also learn how simulation model can be used to study such concepts.

You can also directly access and modify all simulations to adapt them to your research needs.

## Getting Started
The best approach to use this package is to install it, load it, and start the main menu, then you are ready to go. These lines of code typed into the `R` console will get you there:

``` 
install.packages('DSAIDE')
library('DSAIDE')
dsaidemenu()
```

You can also give the package [a quick try online, without having to install it](https://handelgroup.shinyapps.io/dsaide/). I still recommend you install it like any regular `R` package should you decide that you want to use it. Note that I only have a free plan for shinyapps.io, where the online version is hosted. If the link fails to work it likely means I reached my maximum monthly allowed capacity. In that case, just go ahead and install the package. It's only 3 commands! 

For a quick introduction to the package, step-by-step instructions on getting started, and more information on the different ways you can use the package [see the tutorial (vignette)](https://ahgroup.github.io/DSAIDE/articles/DSAIDE.html).

(Also note that DSAIDE doesn't seem to work quite right using Docker. This is likely irrelevant for almost all users.) 

## Further information
* [I published a paper describing the package](https://doi.org/10.1371/journal.pcbi.1005642). The paper is by now already somewhat outdated whith regards to the details of the package, but it describes the overall idea and context well.  
* A 'companion' package to this one, called Dynamical Systems Approaches for Immune Respone Modeling (DSAIRM), focuses on models for analyzing with-host infection dynamics. It has the same structure as DSAIDE. [See the DSAIRM site for more information.](https://ahgroup.github.io/DSAIRM)
* I have solutions (in progress) to most of the 'What to do' tasks for the different apps. If you are an instructor using this package as part of a class, email me if you are interested in having access to these solutions.

## Citation and Contributors
If the package does in any way help you with your work such that it warrants citing in one of your papers, please cite [the DSAIDE publication in PLoS Comp Bio](https://doi.org/10.1371/journal.pcbi.1005642). 

This R package is developed and maintained by [Andreas Handel](https://www.andreashandel.com/). A full list of contributors and a Bibtex entry for the citation [can be found here](https://ahgroup.github.io/DSAIDE/authors.html).