# Startup script
# called at beginning of each documentation Rmd file
# sets various variables, so they don't need to be
# retyped each time

#*************************************
#general setup to define package and get path locations
#all paths are inside the package and retrieved with system.file
packagename = "DSAIDE"
helperdir = "helperfunctions"
mbmodeldir = "mbmodels"
figuredir = "media"
appdocdir = "appinformation"
#*************************************
#Note: for this to process/knit, several helper functions need to be available (sourced) first
#those are in the inst/helperfunctions folder
#Note: in general, the "processing-script.R" should be used to produce the html docs
#manual knitting of each doc only during development/testing
#*************************************
# get_settings.R reads settings from apptable.tsv
# write_tasktext.R writes the text for all tasks into the Rmd/Html file
filenames = c("get_settings.R","write_tasktext.R")
#filenames = c("fill_tasktable.R","get_settings.R","make_tasktable.R","save_tasktable.R","write_tasktext.R")
files_to_source = paste(system.file(helperdir,package = packagename),filenames,sep="/")
#source these files
sapply(files_to_source, source)