#' Setup Julia and EpiAware
#'
#' Configures Julia and installs required Julia packages for EpiAwareR.
#' This function should be run on first use if automatic setup fails.
#'
#' @param verbose Logical. If TRUE, prints progress messages. Default is TRUE.
#'
#' @return Invisible TRUE if setup succeeds, otherwise throws an error.
#'
#' @examples
#' \dontrun{
#' # Setup Julia and install EpiAware packages
#' epiaware_setup_julia()
#' }
#'
#' @export
epiaware_setup_julia <- function(verbose = TRUE) {
  if (verbose) message("[1/7] Setting up Julia...")

  # Setup Julia - will install if not present
  if (verbose) message("[2/7] Calling JuliaCall::julia_setup()...")
  JuliaCall::julia_setup(installJulia = TRUE, verbose = verbose)
  if (verbose) message("[3/7] JuliaCall::julia_setup() completed")

  if (verbose) message("[4/7] Installing Julia packages...")

  # Install packages
  if (verbose) message("[4.1/7] Loading Pkg...")
  JuliaCall::julia_eval("using Pkg")
  if (verbose) message("[4.2/7] Pkg loaded")

  # Install EpiAware from GitHub (since it's not a registered package)
  # Find the EpiAware path - it should be in the parent directory
  epiaware_r_path <- system.file(package = "EpiAwareR")

  if (epiaware_r_path == "") {
    # Package not installed, try to find it relative to current location
    epiaware_r_path <- getwd()
  }

  # Try to find EpiAware/EpiAware directory (the Julia package)
  epiaware_julia_path <- file.path(
    dirname(epiaware_r_path), "EpiAware", "EpiAware"
  )

  if (dir.exists(epiaware_julia_path)) {
    if (verbose) {
      message(
        "[4.3/7] Installing EpiAware from local path: ", epiaware_julia_path
      )
    }
    # Install from local development path
    julia_code <- sprintf(
      'Pkg.develop(path="%s")', gsub("\\\\", "/", epiaware_julia_path)
    )
    JuliaCall::julia_eval(julia_code)
    if (verbose) message("[4.4/7] EpiAware installed from local path")
  } else {
    if (verbose) message("[4.3/7] Installing EpiAware from GitHub...")
    # Install from GitHub using PackageSpec for proper subdir handling
    JuliaCall::julia_eval(
      paste0(
        'Pkg.add(PackageSpec(',
        'url="https://github.com/CDCgov/Rt-without-renewal", ',
        'subdir="EpiAware"))'
      )
    )
    if (verbose) message("[4.4/7] EpiAware installed from GitHub")
  }

  # Install other required packages
  if (verbose) message("[4.5/7] Installing Turing...")
  JuliaCall::julia_eval('Pkg.add("Turing")')
  if (verbose) message("[4.6/7] Installing Distributions...")
  JuliaCall::julia_eval('Pkg.add("Distributions")')
  if (verbose) message("[4.7/7] Installing MCMCChains...")
  JuliaCall::julia_eval('Pkg.add("MCMCChains")')

  # Precompile packages to avoid runtime errors
  if (verbose) message("[4.8/7] Precompiling packages (this may take a while)...")
  tryCatch({
    JuliaCall::julia_eval('Pkg.precompile()')
    if (verbose) message("[4.9/7] Precompilation complete")
  }, error = function(e) {
    if (verbose) {
      message("Warning: Precompilation failed: ", conditionMessage(e))
      message("Continuing anyway - packages will compile on first use")
    }
  })

  # Load packages to verify installation
  if (verbose) message("[5/7] Loading Julia packages...")
  if (verbose) message("[5.1/7] Loading EpiAware...")
  JuliaCall::julia_eval("using EpiAware")
  if (verbose) message("[5.2/7] Loading Turing...")
  JuliaCall::julia_eval("using Turing")
  if (verbose) message("[5.3/7] Loading Distributions...")
  JuliaCall::julia_eval("using Distributions")

  # Test
  if (verbose) message("[6/7] Testing Julia connection...")
  test_result <- JuliaCall::julia_eval("1 + 1")

  if (test_result == 2) {
    if (verbose) message("[7/7] Julia setup complete!")
    invisible(TRUE)
  } else {
    stop("Julia setup failed validation test")
  }
}

#' Check if Julia and EpiAware are available
#'
#' Tests whether Julia is configured and EpiAware packages are accessible.
#'
#' @return Logical. TRUE if Julia and EpiAware are available, FALSE otherwise.
#'
#' @examples
#' \dontrun{
#' if (epiaware_available()) {
#'   # Run EpiAware analysis
#' } else {
#'   epiaware_setup_julia()
#' }
#' }
#'
#' @export
epiaware_available <- function() {
  tryCatch(
    {
      # Check if Julia is available and EpiAware is loaded
      JuliaCall::julia_eval("isdefined(Main, :EpiAware)")
    },
    error = function(e) {
      # Julia not available at all
      FALSE
    }
  )
}

#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Try automatic setup
  tryCatch(
    {
      # Setup Julia connection (don't install Julia automatically in .onLoad)
      JuliaCall::julia_setup()

      # Try to load required Julia packages if they exist
      JuliaCall::julia_eval("using EpiAware")
      JuliaCall::julia_eval("using Turing")
      JuliaCall::julia_eval("using Distributions")

      packageStartupMessage("EpiAware Julia backend loaded successfully")
    },
    error = function(e) {
      packageStartupMessage(
        "Julia setup incomplete. Run epiaware_setup_julia() to configure."
      )
    }
  )
}
