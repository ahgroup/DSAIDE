context("test-modelexploration.R")

test_that("test that modelexploration app returns the proper plots",
          {

            modelsettings = list()
            modelsettings$simfunction = 'simulate_SIR_modelexploration'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)

            modelsettings$samples = 10
            modelsettings$parmin = 1e-04
            modelsettings$parmax = 1e-02
            modelsettings$samplepar = "b"
            modelsettings$pardist = "lin"

            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1

            modelsettings$modeltype = '_modelexploration_'
            modelsettings$nplots = 1
            modelsettings$plotscale = 'x'


            result = run_model(modelsettings)

            testthat::expect_is( generate_ggplot(result), "ggplot" )
            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )


            modelsettings$pardist = "log"
            modelsettings$samplepar = "m"
            result = run_model(modelsettings)
            testthat::expect_is( generate_ggplot(result), "ggplot" )

            })

