context("test-run_model.R")


test_that("run_model correctly runs different models",
{
            tfinal = 120
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, tstart = 0, tfinal = tfinal, dt = 0.1, modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_sir_ode'
            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imax = round(max(dplyr::filter(result[[1]]$dat, varnames == "I")$yvals))
            expect_equal(Imax, 163)
            expect_equal(max(result[[1]]$dat$xvals), tfinal)
            expect_is(generate_ggplot(result), "ggplot" )
            expect_is(generate_plotly(result), "plotly" )
            
            tfinal = 120
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, tstart = 0, tfinal = tfinal, dt = 0.1, modeltype = "_discrete_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_sir_discrete'
            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imax = round(max(dplyr::filter(result[[1]]$dat, varnames == "I")$yvals))
            expect_equal(Imax, 165)
            expect_equal(max(result[[1]]$dat$xvals), tfinal)
            expect_is(generate_ggplot(result), "ggplot" )
            expect_is(generate_plotly(result), "plotly" )
            
            #for this dt, the simulation produces NaN and an error message as string should be returned
            modelsettings =  list( dt = 3, modeltype = "_discrete_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_sir_discrete'
            result = run_model(modelsettings)
            expect_is(result, "character")
            
            
            tfinal = 120
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, tstart = 0, tfinal = tfinal, dt = 0.1, modeltype = "_ode_and_discrete_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction =  c('simulate_sir_ode','simulate_sir_discrete')
            result = run_model(modelsettings)
            #check that simulation ran until max time
            Imaxode = round(max(dplyr::filter(result[[1]]$dat, varnames == "I_ode")$yvals))
            expect_equal(Imaxode, 163)
            Imaxdisc = round(max(dplyr::filter(result[[1]]$dat, varnames == "I")$yvals))
            expect_equal(Imaxdisc, 165)
            expect_equal(max(result[[1]]$dat$xvals), tfinal)
            expect_is(generate_ggplot(result), "ggplot" )
            expect_is(generate_plotly(result), "plotly" )
            
            
            
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, m = 0, n = 0, tmax = 100, rngseed = 123, modeltype = "_stochastic_", plotscale = 'y', nplots = 1,  nreps = 1)
            modelsettings$simfunction = 'simulate_sirdemographic_stochastic'
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Sfinal = min(dplyr::filter(result[[1]]$dat, varnames == "S")$yvals)
            expect_equal(Sfinal, 153)
            expect_is(generate_ggplot(result), "ggplot" )
            expect_is(generate_plotly(result), "plotly" )
            
            modelsettings =  list(S = 1000, I = 10, R = 0 , b = 1e-3, g = 0.5, m = 0, n = 0, tmax = 100, rngseed = 123, modeltype = "_stochastic_", plotscale = 'y', nplots = 1,  nreps = 3)
            modelsettings$simfunction = 'simulate_sirdemographic_stochastic'
            result = run_model(modelsettings)
            #check that simulation returned specific value of susceptible at end
            Sfinal = min(dplyr::filter(result[[1]]$dat, varnames == "S")$yvals)
            expect_equal(Sfinal, 143)
            expect_is(generate_ggplot(result), "ggplot" )
            expect_is(generate_plotly(result), "plotly" )
            

            
            
                        
})