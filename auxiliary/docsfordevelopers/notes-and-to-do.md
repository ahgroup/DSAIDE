# To-do list for DSAIDE package

## Code Improvements
* Continue implementing unit tests using the testthat package
* Add checks to parameter settings, don't allow unreasonable (e.g. negative) ones
* Continue streamlining code base to make it more general/modular/flexible
* Fix that some shiny inputs (e.g. "models to run", "log-scale for plot", and "plot engine") are on top of floating task list (tried briefly, wasn't able to)
* Fix/finish download button for each app (currently disabled). This downloads code that reproduces a given scenario.

## Content Improvement
* Clean up and make all model diagrams as nice as possible (wait for flowdiagramr package)

## Apps to be implemented
* More Fitting apps (e.g. those from DSAIRM)
* global warming app /auxiliary/development/globalwarming/
* Add a multi-scale app
* Do those 3 apps, or remove: Parasite model, ID surveillance, and Maternal immunity

## Documentation / Outreach / Advertisement
* Write a vignette explaining how to use DSAIDE (and solutions/quizgrader) for teachers
* Add more content to the "other resources" section
* Make list of ID Epi and related courses, email instructors
* Send announcements at suitable times
* Create more docs
* Find all classes/instructors who could use package.
* Make videos for levels 1/2/3 for packages 

## General thoughts and comments
* look into R consortium package certification
* Get best practices badge: https://bestpractices.coreinfrastructure.org/en
* Maybe submit for Ropensci review: https://github.com/ropensci/onboarding
* Add/cite/connect to Ottar's book and package: https://github.com/objornstad/epimdr
* Hashtags on twitter when promoting DSAIDE app: #rstats, #dynamicalsystems #infectiousdisease #epidemiology #EpiTwitter
* Contribute code to Epirecipes? http://epirecip.es/


## Thoughts on consistent DSAIDE model notation:

* Main textbooks are V&W, K&R, Bjornstadt. Follow them.
* We don't want to use greek labels.
* VW and KR use b_ij to mean transmission to i from j
* KR and BS use gamma for recovery rate
* Natural mortality is mu in KR and BS and m in VW
* VW use b for birth rate, KR use nu
* KR use sigma for latent rate, VW use f

Our notation:
n for birth rate
m for mortality 
b for transmission
b_ij for transmission TO i FROM j 
g_i for transition rate out of a compartment
w for waning immmunity
