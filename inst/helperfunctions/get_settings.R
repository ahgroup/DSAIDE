#helper function that returns app settings for each Rmd doc file

get_settings <- function(currentrmdfile)
{
  appName = gsub("_documentation.Rmd" ,"",currentrmdfile)
  #find path to apps
  appdir = here::here("inst/appinformation")
  #load table that has all the app information
  at = read.table(file = paste0(appdir,"/apptable.tsv"), sep = '\t', header = TRUE)
  appsettings = as.list(at[which(at$shorttitle == appName),])
  #a few apps have 2 simulator functions, combine here into vector
  if (length(appsettings$simfunction2) > 0) {appsettings$simfunction = c(appsettings$simfunction,appsettings$simfunction2)}

  return(appsettings)

}
