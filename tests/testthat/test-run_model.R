context("test-run_model.R")



test_that("run_model correctly runs different models",
          {
            #####################################################
            #test SIRSd ode model
            modelsettings = list()
            modelsettings$simfunction = 'simulate_SIRSd_model_ode'
            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)
            modelsettings$tfinal = 120
            modelsettings$S = 1000
            modelsettings$I = 10
            modelsettings$b = 1e-3
            modelsettings$g = 0.5
            modelsettings$w = 0
            modelsettings$modeltype = "_ode_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1
            modelsettings$nreps = 1

            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imax = round(max(dplyr::filter(result[[1]]$dat, varnames == "I")$yvals))
            expect_equal(Imax, 163)
            expect_equal(max(result[[1]]$dat$xvals), modelsettings$tfinal)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")

            #####################################################
            #test SIRSd stochastic model

            modelsettings$simfunction = 'simulate_SIRSd_model_stochastic'
            modelsettings$rngseed = 123
            modelsettings$modeltype = "_stochastic_"
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Sfinal = min(dplyr::filter(result[[1]]$dat, varnames == "S")$yvals)
            expect_equal(Sfinal, 153)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")


            #####################################################
            #test running both ode and discrete SIR model
            modelsettings = list()

            modelsettings$simfunction =  c('simulate_SIR_model_ode', 'simulate_SIR_model_discrete')
            modelsettings$modeltypeUI = "_ode_and_discrete_"
            modelsettings$modeltype = "_ode_and_discrete_"
            modelsettings$S = 1000
            modelsettings$I = 10
            modelsettings$R = 0
            modelsettings$b = 1e-3
            modelsettings$g = 0.5
            modelsettings$tstart = 0
            modelsettings$tfinal = 120
            modelsettings$dt = 0.1
            modelsettings$nplots = 1
            modelsettings$nreps = 1


            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imaxode = round(max(dplyr::filter(result[[1]]$dat, varnames == "I_ode")$yvals))
            expect_equal(Imaxode, 163)
            Imaxdisc = round(max(dplyr::filter(result[[1]]$dat, varnames == "I_dis")$yvals))
            expect_equal(Imaxdisc, 165)
            expect_equal(max(result[[1]]$dat$xvals), modelsettings$tfinal)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")

            #####################################################
            #test running both discrete SIR model
            #for this dt, the simulation produces NaN and an error message as string should be returned
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_SIR_model_discrete'
            #set default parameters for input
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$dt = 3
            modelsettings$modeltype = "_discrete_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1

            result = run_model(modelsettings)
            expect_is(result, "character")

            #####################################################
            #test running stochastic SEIRSd model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_SEIRSd_model_stochastic'

            #set default parameters for input
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$S = 1000
            modelsettings$I = 10
            modelsettings$R = 0
            modelsettings$b = 1e-3
            modelsettings$gI = 0.5
            modelsettings$w = 0
            modelsettings$tstart = 0
            modelsettings$tfinal = 120
            modelsettings$dt = 0.1
            modelsettings$modeltype = "_stochastic_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1
            modelsettings$rngseed = 123


            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imax = round(max(dplyr::filter(result[[1]]$dat, varnames == "I")$yvals))
            expect_equal(Imax, 113)
            expect_equal(max(result[[1]]$dat$xvals), modelsettings$tfinal)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")

            #####################################################
            #test running drug resistance and evolutoin model

            modelsettings =  list()
            modelsettings$simfunction = 'simulate_Drug_Resistance_Evolution_stochastic'

            #set default parameters for input
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$S = 1000
            modelsettings$Iu = 1
            modelsettings$R = 0
            modelsettings$gIu = 2
            modelsettings$rngseed = 123
            modelsettings$tstart = 0
            modelsettings$tfinal = 120
            modelsettings$dt = 0.1
            modelsettings$modeltype = "_stochastic_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1

            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imax = round(max(dplyr::filter(result[[1]]$dat, varnames == "Iu")$yvals))
            expect_equal(Imax, 86)
            expect_equal(max(result[[1]]$dat$xvals), modelsettings$tfinal)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")


            #####################################################
            #test running multi outbreak model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_idcontrolmultioutbreak_ode'
            #set default parameters for input
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)

            modelsettings$tnew = 150
            modelsettings$tmax = 100
            modelsettings$modeltype =  "_ode_"

            result = run_model(modelsettings)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")

            #####################################################
            #test running vector transmission model
            modelsettings =  list()
            modelsettings$simfunction = 'simulate_Vector_transmission_model_ode'

            #set default parameters for input
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings, defpar)
            modelsettings$Ih = 10
            modelsettings$b1 = 0.002
            modelsettings$b2 = 0.002
            modelsettings$g = 1
            modelsettings$m = 2
            modelsettings$n = 2000
            modelsettings$modeltype = "_ode_"

            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Sfinal = round(min(dplyr::filter(result[[1]]$dat, varnames == "Sh")$yvals))
            expect_equal(Sfinal, 231)
            expect_is(generate_ggplot(result), "ggplot")
            expect_is(generate_plotly(result), "plotly")
            testthat::expect_is(generate_text(result), "html")


          })
