context("test-generate_documentation.R")


test_that("generate_documentation correctly produces a results needed for the doc tabs",
{
            
            packagename = 'DSAIDE'
            appdir = system.file("appinformation", package = packagename) #find path to apps
            currentapp = "reproductivenumber2"
            currentdocfilename <<- paste0(appdir,'/',currentapp,'_documentation.html')
            docs = generate_documentation(currentdocfilename)
            
            #these elements of the tag list needs to contain the indicated words 
            expect_true(grepl("Overview",docs[[1]][[2]]$title))
            expect_true(grepl("reproductive",docs[[1]][[3]]))
})