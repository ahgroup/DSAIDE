context("test-modelexploration.R")

test_that("test that modelexploration app returns the proper plots",
          {

            modelsettings = list()

            modelsettings$S = 1000
            modelsettings$I = 1
            modelsettings$R = 0
            modelsettings$b = 0.002
            modelsettings$g = 1
            modelsettings$m = 0
            modelsettings$n = 0
            
            modelsettings$tstart = 0
            modelsettings$tfinal = 100
            modelsettings$dt = 0.1

            modelsettings$samples = 10
            modelsettings$parmin = 1e-04
            modelsettings$parmax = 1e-02
            modelsettings$samplepar = "b"
            modelsettings$pardist = "lin"


            modelsettings$rngseed = 100
            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1

            modelsettings$modeltype = '_modelexploration_'
            modelsettings$nplots = 1
            modelsettings$simfunction = 'simulate_modelexploration_sir'
            modelsettings$plotscale = 'x'

            result = run_model(modelsettings)
            
            testthat::expect_is( generate_ggplot(result), "ggplot" )
            testthat::expect_is( generate_plotly(result), "plotly" )
            testthat::expect_is( generate_text(result), "html" )
            testthat::expect_is( generate_text(result), "character" )
            
            })

