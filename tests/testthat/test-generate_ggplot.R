context("test-generate_ggplot.R")

test_that("generate_ggplot returns a ggplot",
          {
            simresult=DSAIDE::simulate_sir_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            outplot = DSAIDE::generate_ggplot(result)
            expect_is(outplot, "ggplot" )
          })

