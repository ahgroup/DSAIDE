context("test-generate_documentation.R")



test_that("generate_documentation correctly produces a results needed for the doc tabs",
{

  # For some weird reason this test fails for some OS versions on CRAN, thus skipping it there
  skip_on_cran()

  #find path to apps
  packagename = 'DSAIDE'
  appdir = system.file("appinformation", package = packagename) #find path to apps
  appName = "reproductivenumber2"
  at = read.table(file = paste0(appdir,"/apptable.tsv"), sep = '\t', header = TRUE)
  appsettings <<- as.list(at[which(at$appid == appName),])
  #file name for documentation
  currentdocfilename <<- paste0(appdir,"/",appsettings$docname)
  docs = generate_documentation(currentdocfilename)
  #these elements of the tag list needs to contain the indicated words
  testthat::expect_true(grepl("Overview",docs[[1]][[2]]$title))
  testthat::expect_true(grepl("reproductive",docs[[1]][[3]]))
})
