
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DSAIDE <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/DSAIDE)](https://cran.r-project.org/package=DSAIDE)
[![CRAN
checks](https://badges.cranchecks.info/worst/DSAIDE.svg)](https://cran.r-project.org/web/checks/check_results_DSAIDE.html)
[![R-CMD-check](https://github.com/ahgroup/DSAIDE/workflows/R-CMD-check/badge.svg)](https://github.com/ahgroup/DSAIDE/actions)
[![Coverage
status](https://codecov.io/gh/ahgroup/DSAIDE/branch/master/graph/badge.svg?token=OGO3sVEcPD)](https://codecov.io/gh/ahgroup/DSAIDE)
[![test-coverage](https://github.com/ahgroup/DSAIDE/workflows/test-coverage/badge.svg)](https://github.com/ahgroup/DSAIDE/actions)
[![metacran monthly
downloads](https://cranlogs.r-pkg.org/badges/DSAIDE)](https://cran.r-project.org/package=DSAIDE)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/DSAIDE?color=ff69b4)](https://cran.r-project.org/package=DSAIDE)
<!-- badges: end -->

**DSAIDE - Dynamical Systems Approach to Infectious Disease Epidemiology
(Ecology/Evolution).**

## Description

DSAIDE is an R package containing a set of simulation models (apps) that
teach infectious disease epidemiology (ecology/evolution) from a
dynamical systems modeling perspective.

All models can be explored through a graphical user interface, no
reading or writing code is required. Each app comes with documentation
and instructions which teach important concepts of infectious disease
epidemiology (ecology/evolution) and show how to use simulation models
to understand such concepts.

It is also possible to go beyond the graphical interface and directly
access and modify all simulations to adapt them to your needs.

## Getting Started

While the main idea is to install the R package and use it locally, if
you want to get a quick glimpse at the package to see if this package is
for you, you can give it [a quick try online, without having to install
it](https://shiny.ovpr.uga.edu/DSAIDE/). If you like what you see, you
can install it and start using it with these 3 commands:

    install.packages('DSAIDE')
    library('DSAIDE')
    dsaidemenu()

For an introduction to the package, step-by-step instructions on getting
started, and more information on the different ways you can use the
package [see the *Get Started* tutorial
(vignette)](https://ahgroup.github.io/DSAIDE/articles/DSAIDE.html).

## Further information

- The [package tutorial
  (vignette)](https://ahgroup.github.io/DSAIDE/articles/DSAIDE.html)
  contains detailed instructions on the different ways the package can
  be used.
- [I published a paper describing the
  package](https://doi.org/10.1371/journal.pcbi.1005642). The package
  has since been updated and changed, but the paper still describes the
  overall idea and context well.  
- I regularly teach two courses related to infectious diseases and
  modeling. All materials for those courses [are freely available
  online](https://andreashandel.github.io/IDEMAcourse/).
- As part of these courses, [I wrote a freely available online
  textbook](https://andreashandel.github.io/IDEMAbook/). It is not (and
  probably never will be) finished, some chapters are fairly empty, but
  some topics are covered in enough detail that I use it for teaching.
- I have full solutions and quiz sheets for all of the **What to do**
  tasks for each app. If you are an instructor using this package as
  part of a class, email me if you are interested in having access to
  the materials.
- Contributions to the package are very welcome! If you want to take a
  deeper look at the package, see [this Markdown
  file](https://github.com/ahgroup/DSAIDE/blob/master/auxiliary/docsfordevelopers/documentation.md)
  which provides further information on the details of the package
  structure. I’d be excited to receive any contributions from
  individuals who want to help improve the package. If you plan to
  develop new apps, or make other substantial contributions, it might be
  best to get in touch with me first.
- A companion package to this one, called *Dynamical Systems Approaches
  for Immune Response Modeling (DSAIRM)*, focuses on models for
  analyzing with-host infection dynamics. It has the same structure as
  DSAIDE. [See the DSAIRM site for more
  information.](https://ahgroup.github.io/DSAIRM/)

## Citation and Contributors

If the package does in any way help you with your work such that it
warrants citing it, please cite [this publication in PLoS Computational
Biology](https://doi.org/10.1371/journal.pcbi.1005642).

This R package is developed and maintained by [Andreas
Handel](https://www.andreashandel.com/). A full list of contributors and
a Bibtex entry for the citation [can be found
here](https://ahgroup.github.io/DSAIDE/authors.html).

This project was/is partially supported by NIH grants U19AI117891,
U01AI150747, R01AI170116, R25AI147391 and R25GM089694.
