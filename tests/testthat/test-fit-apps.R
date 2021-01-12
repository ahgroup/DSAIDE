context("test-fit-apps.R")


test_that("fit apps all run correctly",
{

            #test basic fit app
            modelsettings = list()
            modelsettings$simfunction = 'simulate_flu_fit'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)

            #change settings as needed for testing
            modelsettings$iter = 5
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$usesimdata = 0
            modelsettings$plotscale = 'y'
            modelsettings$solvertype = 1


            result = run_model(modelsettings)
            finaldatapoint = tail(result[[1]]$dat$yvals,1)
            testthat::expect_equal(finaldatapoint, 995500)

            modelsettings$usesimdata = 1
            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )

            #test model comparison fit app
            modelsettings = list()
            modelsettings$simfunction = 'simulate_noro_fit'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)

            modelsettings$fitmodel = 1
            modelsettings$iter = 5
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1
            modelsettings$plotscale = 'both'


            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )
            testthat::  expect_is(generate_plotly(result), "plotly" )


            #########################
            #test model comparison fit app, 2nd model
            modelsettings = list()
            modelsettings$simfunction = 'simulate_noro_fit'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)
            modelsettings$fitmodel = 2
            modelsettings$iter = 5
            modelsettings$solvertype = 2
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1

            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )
            testthat::  expect_is(generate_plotly(result), "plotly" )

            #########################
            #test model comparison fit app, 3rd model
            modelsettings = list()
            modelsettings$simfunction = 'simulate_noro_fit'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)
            modelsettings$fitmodel = 3
            modelsettings$iter = 5
            modelsettings$solvertype = 3
            modelsettings$modeltype = "_fit_"
            modelsettings$nplots = 1


            result = run_model(modelsettings)
            testthat::  expect_is(generate_ggplot(result), "ggplot" )
            testthat::  expect_is(generate_plotly(result), "plotly" )
            testthat::  expect_is(generate_text(result), "html" )

            #test fit app - this one should not work
            modelsettings =  list()
            result = run_model(modelsettings)
            testthat::expect_equal(result, "List element simfunction must be provided.")

})
