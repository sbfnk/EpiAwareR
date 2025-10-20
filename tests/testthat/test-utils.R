# Tests for utility functions and generic wrapper

skip_if_no_julia <- function() {
  testthat::skip_if_not(epiaware_available(), "Julia/EpiAware not available")
}

# Generic Wrapper -------------------------------------------------------

test_that("epiaware_call creates generic model object", {
  skip_if_no_julia()

  # This test assumes there's some generic Julia function we can call
  # In practice, we'd call an actual EpiAware function
  skip("Requires specific Julia function to test")
})

test_that("epiaware_call validates function name", {
  skip_if_no_julia()

  expect_error(
    epiaware_call(123, param = 1),
    "fn_name must be a character string"
  )

  expect_error(
    epiaware_call("", param = 1),
    "fn_name must be a non-empty character string"
  )
})

test_that("epiaware_call allows custom class specification", {
  skip_if_no_julia()
  skip("Requires specific Julia function to test")
})

# Internal Helpers ------------------------------------------------------

test_that(".prepare_data_for_julia handles data frames", {
  skip_if_no_julia()

  # Test with y_t column
  df1 <- data.frame(y_t = 1:10, other_col = runif(10))
  prepared1 <- .prepare_data_for_julia(df1, c(1, 10))

  expect_type(prepared1, "list")
  expect_true("y_t" %in% names(prepared1))

  # Test with cases column
  df2 <- data.frame(cases = 1:10, other_col = runif(10))
  prepared2 <- .prepare_data_for_julia(df2, c(1, 10))

  expect_type(prepared2, "list")
  # cases should be renamed to y_t for Julia
})

test_that(".prepare_data_for_julia handles lists", {
  skip_if_no_julia()

  list_data <- list(
    y_t = 1:10,
    dates = seq.Date(as.Date("2020-01-01"), by = "day", length.out = 10)
  )
  prepared <- .prepare_data_for_julia(list_data, c(1, 10))

  expect_type(prepared, "list")
  expect_true("y_t" %in% names(prepared))
})

test_that(".prepare_data_for_julia validates data length", {
  skip_if_no_julia()

  # Data length doesn't match tspan
  df <- data.frame(y_t = 1:5)

  expect_error(
    .prepare_data_for_julia(df, c(1, 10)),
    "Data length.*is less than.*tspan"
  )
})

test_that(".compute_diagnostics calculates Rhat and ESS", {
  skip_if_no_julia()
  skip("Requires posterior draws object")
})
