#' Setup specific Julia version using juliaup
#'
#' Installs and configures a specific Julia version using juliaup if available.
#' Returns the path to the Julia bin directory, suitable for use as
#' `JULIACONNECTOR_JULIABIN` (set by the caller via [Sys.setenv()]).
#'
#' @param version Character string with the required Julia version (e.g., "1.11").
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return Path to the Julia executable (not the bin directory), or `NULL`
#'   if juliaup is not available or installation failed.
#'
#' @keywords internal
.setup_julia_version <- function(version, verbose = TRUE) {
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

  julia_base <- path.expand("~/.julia/juliaup")
  julia_dirs <- list.dirs(julia_base, recursive = FALSE, full.names = TRUE)
  version_pattern <- paste0("julia-", version)
  matching_dirs <- julia_dirs[grepl(version_pattern, julia_dirs)]

  if (length(matching_dirs) == 0) {
    if (verbose) message("Could not find Julia ", version, " installation path")
    return(NULL)
  }

  julia_bin <- file.path(matching_dirs[1], "bin", "julia")
  if (file.exists(julia_bin)) {
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

# Environment to track Julia initialisation state
.epiaware_env <- new.env(parent = emptyenv())

#' Setup Julia and EpiAware
#'
#' Configures Julia and installs required Julia packages for EpiAwareR.
#' If a `julia` version is pinned in upstream EpiAware's Project.toml, that
#' version is installed via [juliaup](https://github.com/JuliaLang/juliaup)
#' and used; otherwise the system Julia is used.
#'
#' Heavy lifting (subprocess install, lazy-init guard) is delegated to
#' [juliaready::julia_ready()].
#'
#' @param verbose Logical. If TRUE, prints progress messages.
#' @return Invisible TRUE on success.
#'
#' @examples
#' \dontrun{
#' epiaware_setup_julia()
#' }
#'
#' @export
epiaware_setup_julia <- function(verbose = TRUE) {
  # Pin the Julia binary if upstream specifies a version, then juliaready
  # picks it up via the JULIACONNECTOR_JULIABIN env var.
  required_version <- .get_epiaware_julia_version()
  if (!is.null(required_version)) {
    if (verbose) message("EpiAware requires Julia ", required_version)
    pinned_bin <- .setup_julia_version(required_version, verbose)
    if (!is.null(pinned_bin)) {
      Sys.setenv(JULIACONNECTOR_JULIABIN = pinned_bin)
    }
  }

  # If a sibling EpiAware/EpiAware/ checkout exists (development workflow),
  # prefer it over the GitHub URL.
  epiaware_r_path <- system.file(package = "EpiAwareR")
  if (epiaware_r_path == "") epiaware_r_path <- getwd()
  local_julia_path <- file.path(
    dirname(epiaware_r_path), "EpiAware", "EpiAware"
  )
  if (dir.exists(local_julia_path)) {
    if (verbose) message("Using local EpiAware checkout: ", local_julia_path)
    # Install via develop in a subprocess so the in-process server doesn't
    # see a Pkg.activate(path).
    bin <- juliaready::julia_bin()
    juliaready:::julia_subprocess(
      sprintf(
        'import Pkg; Pkg.develop(path="%s"); using EpiAware',
        gsub("\\\\", "/", local_julia_path)
      ),
      bin = bin
    )
  }

  juliaready::julia_ready(
    packages  = c("EpiAware", "Turing", "Distributions",
                  "MCMCChains", "Pathfinder", "ADTypes"),
    github    = c(EpiAware = "CDCgov/Rt-without-renewal:EpiAware"),
    state_env = .epiaware_env,
    install   = TRUE,
    verbose   = verbose
  )

  if (verbose) message("EpiAwareR Julia backend ready")
  invisible(TRUE)
}

#' Check if Julia and EpiAware are available
#'
#' @return Logical. `TRUE` if Julia and EpiAware are available, `FALSE` otherwise.
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
  isTRUE(.epiaware_env$ready) &&
    tryCatch(
      juliaready::eval_julia("isdefined(Main, :EpiAware)"),
      error = function(e) FALSE
    )
}

#' Initialise Julia lazily (on first use)
#'
#' @return Invisibly `TRUE` on success.
#' @keywords internal
.ensure_julia_initialized <- function() {
  juliaready::ensure_julia(.epiaware_env, function() {
    epiaware_setup_julia(verbose = FALSE)
  })
}

#' Package load hook
#'
#' @param libname Character string. Library directory where the package is
#'   installed.
#' @param pkgname Character string. The package name.
#'
#' @return `NULL` (called for side effects).
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Lazy: do not initialise Julia on package load. Eager init in .onLoad
  # interacts badly with other compiled backends (notably Stan) and can
  # crash R during attach.
}
