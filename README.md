[![R build status](https://github.com/ahgroup/DSAIDE/workflows/R-CMD-check/badge.svg)](https://github.com/ahgroup/DSAIDE/actions)
[![Coverage status](https://codecov.io/gh/ahgroup/DSAIDE/branch/master/graph/badge.svg?token=OGO3sVEcPD)](https://codecov.io/gh/ahgroup/DSAIDE)
[![CRAN status](https://www.r-pkg.org/badges/version/DSAIDE)](https://cran.r-project.org/package=DSAIDE)
[![CRAN checks](https://cranchecks.info/badges/summary/DSAIDE)](https://cran.r-project.org/web/checks/check_results_DSAIDE.html)
[![metacran monthly downloads](http://cranlogs.r-pkg.org/badges/DSAIDE)](https://cran.r-project.org/package=DSAIDE)
[![metacran downloads](http://cranlogs.r-pkg.org/badges/grand-total/DSAIDE?color=ff69b4)](https://cran.r-project.org/package=DSAIDE)


# DSAIDE
Dynamical Systems Approach to Infectious Disease Epidemiology (Ecology/Evolution)

Find the website for the package [here.](https://ahgroup.github.io/DSAIDE/)

## Description
DSAIDE is an R package containing a set of simulation models (apps) that teach infectious disease epidemiology (ecology/evolution) from a dynamical system perspective. 

All models can be explored through a graphical user interface, no reading or writing code is required. Each app comes with documenation and instructions which teach important concepts of infectious disease epidemiology (ecology/evolution) and how to use simulation models to understand such concepts. 

It is also possible to go beyond the graphical interface and directly access and modify all simulations to adapt them to your needs.

## Getting Started
For a quick introduction to the package, step-by-step instructions on getting started, and more information on the different ways you can use the package [see the "Get Started" tutorial (vignette)](https://ahgroup.github.io/DSAIDE/articles/DSAIDE.html).

If you are very keen to get started quickly and don't want to read the "Get Started" guide right now, here are the commands to get the package installed, loaded and the main menu started:

``` 
install.packages('DSAIDE')
library('DSAIDE')
dsaidemenu()
```

You can also give the package [a quick try online, without having to install it](https://shiny.ovpr.uga.edu/DSAIDE/). I still recommend you install it like any regular `R` package should you decide that you want to use it. The online version is likely not quite as robust as the local. So just go ahead and install the package, it's only 3 commands to get it up and running! 

## Further information
* [I published a paper describing the package](https://doi.org/10.1371/journal.pcbi.1005642). The paper is by now somewhat outdated with regards to the details of the package, but it describes the overall idea and context well.  
* The [package tutorial (vignette)](https://ahgroup.github.io/DSAIDE/articles/DSAIDE.html) contains up-to-date and detailed instructions on the different ways the package can be used.
* If you want to take a deeper look at the package, see [this Markdown file](https://github.com/ahgroup/DSAIDE/blob/master/inst/docsfordevelopers/documentation.md) which provides further information on the details of the package structure. (If you plan to develop new apps, or make other substantial contributions, it is best to get in touch with me first.)
* A 'companion' package to this one, called Dynamical Systems Approaches for Immune Response Modeling (DSAIRM), focuses on models for analyzing with-host infection dynamics. It has the same structure as DSAIDE. [See the DSAIRM site for more information.](https://ahgroup.github.io/DSAIRM/)
* I have solutions (in progress) to most of the 'What to do' tasks for the different apps. If you are an instructor using this package as part of a class, email me if you are interested in having access to these solutions.
* I regularly teach two courses related to infectious diseases and modeling. The website with materials for both courses is [here](https://andreashandel.github.io/IDEMAcourse/). 

## Citation and Contributors
If the DSAIDE package does in any way help you with your work such that it warrants citing it in one of your papers, please cite [the DSAIDE publication in PLoS Comp Bio](https://doi.org/10.1371/journal.pcbi.1005642). 

This R package is developed and maintained by [Andreas Handel](https://www.andreashandel.com/). A full list of contributors and a Bibtex entry for the citation [can be found here](https://ahgroup.github.io/DSAIDE/authors.html).
