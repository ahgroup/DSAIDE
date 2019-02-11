# To-do list for DSAIDE package

## Code Improvements
* Continue implementing unit tests using the testthat package
* Provide a 'download scenario' button, which downloads code that reproduces a given scenario.
* Improve error messages when simulation failed: Add a failure flag to each underlying simulator, have calling function check the failure status and process accordingly
* Add checks to parameter settings, don't allow unreasonable (e.g. negative) ones
* Add a 'reset' button that sets inputs back to function default values
* Match Shiny UI defaults with simulator_ defaults

* Consider changing placement of inputs/outputs/instructions
* Continue streamlining code base to make it more general/modular/flexible

## Content Improvement
* Write/update all solutions
* Add learning objectives to each overview tab
* Implement further apps, see below
* Clean up and make all model diagrams as nice as possible


## Apps to be implemented
* parameter scanning, U/S, fitting apps from DSAIRM
* Implement new apps from 8515 students: 


## General thoughts and comments

* look into R consortium package certification
* Get best practices badge: https://bestpractices.coreinfrastructure.org/en
* Maybe submit for Ropensci review: https://github.com/ropensci/onboarding
* Add/cite/connect to Ottar's book and package: https://github.com/objornstad/epimdr
* Hashtags on twitter when promoting app: #rstats, #dynamicalsystems #infectiousdisease #epidemiology #EpiTwitter


