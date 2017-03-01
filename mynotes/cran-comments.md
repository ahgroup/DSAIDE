## Test environments
* local Windows 10.1 x86_64, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTES:




* checking installed package size ... NOTE
  installed size is  6.9Mb
  sub-directories of 1Mb or more:
    shinyapps   6.0Mb

Each shiny app contains information about the model that is studied. This documentation includes a figure for each app, thus the 6Mb size.
I have excluded the folders containing the files with the figures and all the raw documentation (folders \inst\shinyapps\allappdocumentation and \inst\shinyapps\media) with .Rbuildignore but this does not seem to be considered by R CMD check.



## Submission History
This is the first submission of this package.
