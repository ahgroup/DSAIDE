context("test-generate_usplot.R")

test_that("running US analysis app returns the proper plot",
          {
            
            modelsettings = list()
            
            modelsettings$Smin = 1000
            modelsettings$Smax = 1500
            modelsettings$Imin = 1
            modelsettings$Imax = 10
            modelsettings$bmin = 1e-04
            modelsettings$bmax = 0.01
            modelsettings$gmean = 1
            modelsettings$gvar = 0.1
            modelsettings$mmin = 0
            modelsettings$mmax = 10
            modelsettings$nmin = 0
            modelsettings$nmax = 0.1 
            modelsettings$samples = 10
            modelsettings$rngseed = 100
            modelsettings$tstart = 0
            modelsettings$tfinal = 200
            modelsettings$dt = 0.1
            
            modelsettings$modeltype = '_usanalysis_'
            modelsettings$nplots = 3
            modelsettings$simfunction = 'simulate_usanalysis_sir'
            modelsettings$plotscale = 'y'
            modelsettings$plottype = 'Scatterplot'
            modelsettings$samplepar = 'g'
              
            result = run_model(modelsettings)
            usplot = DSAIDE::generate_ggplot(result)
            expect_is( usplot, "gtable" )
          })

