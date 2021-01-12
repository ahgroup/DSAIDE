context("test-all_simulators.R")


test_that("all simulator functions run correctly when called directly",
{

            sim = simulate_Characteristics_of_ID_ode()
            expect_type(sim,"list")

            sim = simulate_Environmental_Transmission_model_ode()
            expect_type(sim,"list")

            sim = simulate_directtransmission_ode()
            expect_type(sim,"list")

            sim = simulate_idcontrolmultigroup_ode()
            expect_type(sim,"list")

            sim = simulate_idvaccine_ode()
            expect_type(sim,"list")

            #sim = simulate_idsurveillance_ode()
            #expect_type(sim,"list")

            #sim = simulate_parasites_ode()
            #expect_type(sim,"list")

})
