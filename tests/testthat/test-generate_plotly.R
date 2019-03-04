context("test-generate_plotly.R")

test_that("generate_plotly returns a plotly plot",
          {
            simresult = simulate_idpatterns_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( generate_plotly(result), "plotly" )
          })

