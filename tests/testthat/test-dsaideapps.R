context("test-dsaideapps.R")

test_that("dsadeapp() returns character string", {
  expect_true( is.character(DSAIDE::dsaideapps()) )
})

