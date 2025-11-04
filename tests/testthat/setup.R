# Test setup and helpers
# This file is automatically loaded before all tests

# Skip helper for Julia/EpiAware availability
skip_if_no_julia <- function() {
  testthat::skip_if_not(epiaware_available(), "Julia/EpiAware not available")
}
