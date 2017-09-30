## Test environments
* local Windows 10.1 x86_64, R 3.4.1
* ubuntu 12.04 (on travis-ci), R 3.4.1

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking installed package size ... NOTE
  installed size is  6.4Mb
  sub-directories of 1Mb or more:
    shinyapps   6.1Mb

Each shiny app contains information about the model that is studied. This documentation includes a figure for each app, thus the 6Mb size.
I have excluded the folders containing the files with the figures and all the raw documentation (folders \inst\shinyapps\allappdocumentation and \inst\shinyapps\media) from the package with .Rbuildignore but this does not seem to be considered by R CMD check.



## Submission History

version 0.6: bug fixes in apps and documentation, updates to teaching tasks for several apps.

version 0.5: added one new app, lots of bug fixes in other apps and documentation.

version 0.4.: This is the first submission of this package.
