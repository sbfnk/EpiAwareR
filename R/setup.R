#' Setup specific Julia version using juliaup
#'
#' Installs and configures a specific Julia version using juliaup if available.
#'
#' @param version Character string with the required Julia version (e.g., "1.11").
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return Path to the Julia bin directory, or NULL if juliaup is not available.
#'
#' @keywords internal
.setup_julia_version <- function(version, verbose = TRUE) {
  # Check if juliaup is available
  juliaup_path <- Sys.which("juliaup")
  if (juliaup_path == "") {
    if (verbose) {
      message(
        "juliaup not found. Install it from https://github.com/JuliaLang/juliaup"
      )
      message("Falling back to default Julia installation...")
    }
    return(NULL)
  }

  if (verbose) message("Using juliaup to set up Julia ", version, "...")

  # Install the required version
  install_result <- tryCatch(
    {
      system2(
        "juliaup", c("add", version),
        stdout = if (verbose) "" else FALSE,
        stderr = if (verbose) "" else FALSE
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!install_result) {
    if (verbose) message("Failed to install Julia ", version, " via juliaup")
    return(NULL)
  }

  # Get the path to the installed Julia version
  julia_base <- path.expand("~/.julia/juliaup")

  # Find the directory for this version
  julia_dirs <- list.dirs(julia_base, recursive = FALSE, full.names = TRUE)
  version_pattern <- paste0("julia-", version)
  matching_dirs <- julia_dirs[grepl(version_pattern, julia_dirs)]

  if (length(matching_dirs) == 0) {
    if (verbose) message("Could not find Julia ", version, " installation path")
    return(NULL)
  }

  julia_dir <- matching_dirs[1]
  julia_bin <- file.path(julia_dir, "bin")

  if (dir.exists(julia_bin)) {
    if (verbose) message("Found Julia ", version, " at: ", julia_bin)
    return(julia_bin)
  }

  NULL
}

#' Get required Julia version from EpiAware Project.toml
#'
#' Fetches the Julia version compatibility from the upstream EpiAware package.
#'
#' @return Character string with the required Julia version (e.g., "1.11"),
#'   or NULL if it cannot be determined.
#'
#' @keywords internal
.get_epiaware_julia_version <- function() {
  url <- paste0(
    "https://raw.githubusercontent.com/CDCgov/Rt-without-renewal/",
    "main/EpiAware/Project.toml"
  )

  tryCatch(
    {
      lines <- readLines(url, warn = FALSE)
      julia_line <- grep("^julia[[:space:]]*=", lines, value = TRUE)
      if (length(julia_line) > 0) {
        match <- regmatches(
          julia_line,
          regexpr('"[0-9]+\\.[0-9]+(\\.[0-9]+)?"', julia_line)
        )
        if (length(match) > 0) {
          return(gsub('"', "", match))
        }
      }
      NULL
    },
    error = function(e) NULL
  )
}

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

  # Get required Julia version from EpiAware
  required_version <- .get_epiaware_julia_version()
  if (verbose && !is.null(required_version)) {
    message("EpiAware requires Julia ", required_version)
  }

  julia_home <- NULL
  if (!is.null(required_version)) {
    julia_home <- .setup_julia_version(required_version, verbose)
  }

  if (verbose) message("[2/7] Calling JuliaCall::julia_setup()...")
  JuliaCall::julia_setup(
    JULIA_HOME = julia_home,
    installJulia = is.null(julia_home),
    version = required_version,
    verbose = verbose
  )
  if (verbose) message("[3/7] JuliaCall::julia_setup() completed")

  if (verbose) message("[4/7] Installing Julia packages...")

  if (verbose) message("[4.1/7] Loading Pkg...")
  JuliaCall::julia_eval("using Pkg")
  if (verbose) message("[4.2/7] Pkg loaded")

  epiaware_r_path <- system.file(package = "EpiAwareR")
  if (epiaware_r_path == "") {
    epiaware_r_path <- getwd()
  }

  epiaware_julia_path <- file.path(
    dirname(epiaware_r_path), "EpiAware", "EpiAware"
  )

  if (dir.exists(epiaware_julia_path)) {
    if (verbose) {
      message(
        "[4.3/7] Installing EpiAware from local path: ", epiaware_julia_path
      )
    }
    julia_code <- sprintf(
      'Pkg.develop(path="%s")', gsub("\\\\", "/", epiaware_julia_path)
    )
    JuliaCall::julia_eval(julia_code)
    if (verbose) message("[4.4/7] EpiAware installed from local path")
  } else {
    if (verbose) message("[4.3/7] Installing EpiAware from GitHub...")
    JuliaCall::julia_eval(
      paste0(
        'Pkg.add(PackageSpec(',
        'url="https://github.com/CDCgov/Rt-without-renewal", ',
        'subdir="EpiAware"))'
      )
    )
    if (verbose) message("[4.4/7] EpiAware installed from GitHub")
  }

  # Turing, Distributions, MCMCChains are dependencies of EpiAware
  # No need to install them separately

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
  tryCatch(
    {
      required_version <- .get_epiaware_julia_version()
      julia_home <- NULL
      if (!is.null(required_version)) {
        julia_home <- .setup_julia_version(required_version, verbose = FALSE)
      }

      JuliaCall::julia_setup(JULIA_HOME = julia_home)
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
