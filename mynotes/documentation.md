#Documentation for working on the DSAIDE Package

##To work on package in RStudio: 
* Load DSAIDE.Rproj in RStudio. Edit files as needed.
* The main R functions, i.e. the menu and the simulation scripts are in the R/ folder
* Anything in the R/ folder can also be executed directly by the user
* Anything related to the shiny UI/server and accompanying files is under inst/shinyapps
* AppDocContent contains html files for content in shiny apps. This will need to be processed, see below.

###Dependency packages for development
* roxygen
* devtools
* packages needed by DSAIDE (see DESCRIPTION file)

##To prepare shiny documentation:
* Copy html files with content for each shiny app into the AppDocContent folder. 
* First part of file name must be the same as name for the Shiny app. _Practice will be stripped.
* Run the R script processAllFiles, which will execute a function by the same name inside the script. This splits html files. Each section in the html file that has a 'shinytabNNN' id will be extracted and placed into a separate html file. Any other sections are ignored.
* The newly created files will be placed in the www subfolders for each shiny app in inst/shinyapps/
* The www subfolder for each shiny app is deleted and re-created on each run, so should not contain any other files
* Naming for newly created html files is shinytab1.html, etc. and header.html/footer.html
* see comments in processAllFiles.R for more details
* Folder AppDocContent will not be part of the final R package

##To build the DSAIDE package:
* "by hand" edit the DESCRIPTION file to make sure it's up to date
* Build documentation with More/Document or devtools::document()
* Use the functions in the 'build' tab to test and build the package.
* Run clean and rebuild, then build and reload using menu, or devtools::load_all()
* Run the check 
* To make a .gz package, choose 'build source package' in the 'more' tab or type R CMD build DSAIDE in the command line

##To use package for users:
* get the .gz file
* install required packages
* type install.packages('DSAIDE_X.X.tar.gz', repos = NULL, type = "source") or use the RStudio menu
* library('DSAIDE') 
* dsaidemenu()

##Notes:
* One can also use the 'git' tab to sync with github.
* All needed libraries should be loaded via the DESCRIPTION file and not in separate R files