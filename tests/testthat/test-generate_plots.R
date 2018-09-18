context("test-generate_plots.R")

test_that("generate_plots returns a plot",
          {
            simresult=DSAIDE::simulate_SIR_model_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( DSAIDE::generate_plots(result), "ggplot" )
          })

