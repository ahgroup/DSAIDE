#run this script after major changes to do some cleaning and processing automatically
library('here')
library('devtools')
library('pkgdown')
library('zip')



#get path to Rmd files containing documentation
basepath = here::here()
files = list.files(path = paste0(basepath, "/inst/appinformation/"), recursive=TRUE, pattern = "\\.Rmd$", full.names = TRUE)

#run spell check - only turn on if wanted
#spelling::spell_check_files(files)

#re-build all html documentation files from the rmd files at once
for (n in 1: length(files)) {rmarkdown::render(files[n]); Sys.sleep(2)}


# To copy simulator functions into the /inst/simulator folder:
files = list.files(path = paste0(basepath,"/R/"), recursive=TRUE, pattern = "^simulate", full.names = TRUE)
file.copy(files, paste0(basepath,"/inst/simulatorfunctions/"), overwrite = TRUE)

# create zip file
zip::zipr(zipfile = paste0(basepath,"/inst/simulatorfunctions/simulatorfunctions.zip"), files = files, recurse = FALSE, include_directories = FALSE)


# re-build vignette
devtools::build_vignettes()

#update the pkgdown website
pkgdown::build_site()

