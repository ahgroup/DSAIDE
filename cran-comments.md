## Test environments
* local Windows 10.1 x86_64, R 3.5.1
* Ubuntu 14.04 (on travis-ci), R 3.4, release
* MacOS 10.12 (on travis-ci), R 3.4, release
* Windows (on appveyor), R 3.5.1


## R CMD check results

There were no errors and warnings.

There was 1 NOTE:

* checking R code for possible problems ... NOTE
generate_plots: no visible binding for global variable 'xvals'

And several others of that sort. Seems to come from various dplyr and ggplot commands. Based on an online search, this seems to be a common issue. I found several suggested solutions to getting rid of the notes, but they all seemed like hacks, and it doesn't seem to affect anything, so I chose to ignore these notes.




## Submission History

version 0.7: major revision of the underlying way plots and text are produced. now uses ggplot2. updates to all functions and instructios. several new apps.

version 0.6: bug fixes in apps and documentation, updates to teaching tasks for several apps.

version 0.5: added one new app, lots of bug fixes in other apps and documentation.

version 0.4.: This is the first submission of this package.
