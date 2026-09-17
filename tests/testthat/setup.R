# Julia-dependent tests start the backend once and are skipped if Julia or
# its packages are unavailable.
skip_if_no_julia <- function() {
  testthat::skip_on_cran()
  if (!epiaware_available()) {
    ok <- tryCatch(
      {
        epiaware_setup_julia(verbose = FALSE)
        TRUE
      },
      error = function(e) FALSE
    )
    testthat::skip_if_not(ok, "Julia backend not available")
  }
}
