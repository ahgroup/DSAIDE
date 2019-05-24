context("test-generate_plotly.R")

test_that("generate_plotly returns a plotly plot",
          {
            simresult = simulate_idpatterns_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( generate_plotly(result), "plotly" )
            
            simresult = simulate_idcontrol_ode()
            result = vector("list", 1)
            result[[1]]=simresult
            expect_is(generate_plotly(result), "plotly" )
            
            simresult = simulate_multipathogen_ode()
            result = vector("list", 1)
            result[[1]]=simresult
            result[[1]]$title = "Hello"
            result[[1]]$legendlocation = "left"
            expect_is(generate_plotly(result), "plotly" )
            
            modelsettings =  list(modeltype = "_ode_", plotscale = 'y', nplots = 1)
            modelsettings$simfunction = 'simulate_reproductivenumber2_ode'
            result = run_model(modelsettings)
            expect_is(generate_plotly(result), "plotly" )
            
            
          })

