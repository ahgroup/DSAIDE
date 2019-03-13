[![Travis-CI build Status](https://travis-ci.org/ahgroup/DSAIDE.svg?branch=master)](https://travis-ci.org/ahgroup/DSAIDE)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/ahgroup/DSAIDE?branch=master&svg=true)](https://ci.appveyor.com/project/ahgroup/DSAIDE)
[![Coverage status](https://codecov.io/gh/ahgroup/DSAIDE/branch/master/graph/badge.svg)](https://codecov.io/github/ahgroup/DSAIDE?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/DSAIDE)](https://cran.r-project.org/package=DSAIDE)

# DSAIDE
Dynamical Systems Approach to Infectious Disease Epidemiology

## Description
This R package consists of a set of simulations (refered to here as apps) that teach infectious disease epidemiology from a dynamical system perspective. By manipulating the models through the Shiny UI interface and working through the instructions provided within the Shiny UI, you can learn about some important concepts in infectious disease epidemiology. 
You will also learn how models can be used to study such concepts.

## Installation
I assume you have `R` installed. I also highly recommend `RStudio`, though it's not required.

1. Install the CRAN release in the usual way with `install.packages('DSAIDE')`.
2. The latest development version (more features but potentially buggy) can be installed from github, using the `devtools` package. If you don't have it, install it first. The following commands will get you up and running:

```r
install.packages('devtools') #if not already isntalled
devtools::install_github('ahgroup/DSAIDE')
```

## Basic Use
After install (which you need to do only once), load the package by running `library('DSAIDE')`. You should receive a short greeting. Now you can open the main menu by running `dsaidemenu()`. From the main menu, choose the different apps corresponding to different modeling topics and scenarios. Each app contains information on the model and topic that are covered. Each app also contains a list of recommeded tasks to work through in order to learn about a specific topic. Once done exploring, close the main menu to exit back to the `R` console.

## Advanced Use
You can call the underlying simulation functions directly from the `R` console. You can also download the code for all functions from the main menu and modify them to suit your own needs. See [the package vignette](https://ahgroup.github.io/DSAIDE/articles/DSAIDE.html) for more details on the different ways to use the package. 

## Contributing to the package
The package is on GitHub and you can use the usual GitHub process to file bug reports, send feature requests, contribute updates and bug fixes, etc. If you have any comments or feedback, I very much welcome them. Please file a [GitHub issue](https://github.com/ahgroup/DSAIDE/issues) and let me know what you think.

The package is built in a way that makes it (hopefully) easy for others to contribute new simulations/apps. To that end, the package contains [this Markdown file](https://github.com/ahgroup/DSAIDE/blob/master/inst/docsfordevelopers/documentation.md) which provides further information on the details of the package structure. If you plan to develop new apps, or make other substantial contributions, it's best to get in touch with me first via email or GitHub.

## Further information
* [I published a paper describing the package and how to use it](https://doi.org/10.1371/journal.pcbi.1005642). The paper is by now already somewhat outdated when regarding the details of the package, but it describes the overall idea and context well, if you are interested in that.  
* A 'companion' package to this one, called Dynamical Systems Approaches for Immune Respone Modeling (DSAIRM), focuses on models for analyzing with-host infection dynamics. It has the same structure as DSAIDE. [See the DSAIRM site for more information.](https://ahgroup.github.io/DSAIRM)
* I have solutions (as R Markdown files) to most of the 'What to do' tasks for the different apps. If you are an instructor using this package as part of a class, email me if you are interested in having access to these solutions.

## Citation and Contributors
This R package is developed and maintained by [Andreas Handel](http://handelgroup.uga.edu/). 

If the package does in any way help you with your work such that it warrants citing in one of your papers, please cite [the DSAIDE publication in PLoS Comp Bio](https://doi.org/10.1371/journal.pcbi.1005642). 

A full list of contributors and a Bibtex entry for the citation [can be found here](https://ahgroup.github.io/DSAIDE/authors.html).
