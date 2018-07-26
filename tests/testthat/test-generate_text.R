context("test-generate_text.R")


test_that("generate_text returns text string",
          {
            simresult=DSAIDE::simulate_idcharacteristics()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            #should both be of class html and character
            expect_is( DSAIDE::generate_text(result), "html" )
            expect_is( DSAIDE::generate_text(result), "character" )
          })

