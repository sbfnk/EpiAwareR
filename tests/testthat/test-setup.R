# Tests for Julia setup and initialization

test_that("epiaware_available returns logical", {
  result <- epiaware_available()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("epiaware_setup_julia runs without error", {
  skip_on_cran()
  testthat::skip_if_not(interactive(), "Setup only in interactive mode")

  expect_error(
    epiaware_setup_julia(verbose = FALSE),
    NA
  )
})

test_that("Julia availability can be checked", {
  # Should not throw error even if Julia not available
  expect_no_error(epiaware_available())
})
