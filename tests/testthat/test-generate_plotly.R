context("test-generate_plotly.R")

test_that("generate_plotly returns a plotly plot",
          {
            simresult = simulate_idpatterns_ode()
            result = vector("list", 1)
            result[[1]]$dat = simresult$ts
            expect_is( generate_plotly(result), "plotly" )

            simresult = simulate_Host_Heterogeneity_Model_ode()
            result = vector("list", 1)
            result[[1]]=simresult
            expect_is(generate_plotly(result), "plotly" )

            simresult = simulate_multipathogen_ode()
            result = vector("list", 1)
            result[[1]]=simresult
            result[[1]]$title = "Hello"
            result[[1]]$legendlocation = "left"
            expect_is(generate_plotly(result), "plotly" )

            modelsettings =  list()
            modelsettings$simfunction = 'simulate_Characteristics_of_ID_ode'

            #use default values for simulation function,
            #they need to be part of modelsettings otherwise run_model won't work
            defpar = formals(modelsettings$simfunction)
            modelsettings = c(modelsettings,defpar)

            modelsettings$modeltype = "_ode_"
            modelsettings$plotscale = 'y'
            modelsettings$nplots = 1


            result = run_model(modelsettings)
            expect_is(generate_plotly(result), "plotly" )


          })

