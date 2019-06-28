context("test-fit-apps.R")


test_that("fit apps all run correctly",
{

            #test basic fit app
            modelsettings = list()
            modelsettings$iter = 5
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$usesimdata = 0
            modelsettings$plotscale = 'y'
            modelsettings$solvertype = 1
            modelsettings$simfunction = 'simulate_fit_flu'

            result = run_model(modelsettings)
            finaldatapoint = tail(result[[1]]$dat$yvals,1)
            testthat::expect_equal(finaldatapoint, 995500)

            modelsettings$usesimdata = 1
            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )

            #test model comparison fit app
            modelsettings =  list(fitmodel = 1, iter = 5)
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$plotscale = 'both'
            modelsettings$simfunction = 'simulate_fit_noro'
            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )
            testthat::  expect_is(generate_plotly(result), "plotly" )

            #test model comparison fit app, 2nd model
            modelsettings =  list(fitmodel = 2, iter = 5, solvertype = 2)
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_fit_noro'
            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )
            testthat::  expect_is(generate_plotly(result), "plotly" )

            #test model comparison fit app, 2nd model
            modelsettings =  list(fitmodel = 3, iter = 5, solvertype = 3)
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_fit_noro'
            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )
            testthat::  expect_is(generate_plotly(result), "plotly" )
            testthat::  expect_is(generate_text(result), "html" )
            
            #test fit app - this one should not work
            modelsettings =  list()
            result = run_model(modelsettings)
            testthat::expect_equal(result, "List element simfunction must be provided.")

})
