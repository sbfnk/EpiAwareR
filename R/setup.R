# Environment tracking Julia initialisation and fitted-model handles.
.epiaware_env <- new.env(parent = emptyenv())

#' Locate the bundled Julia project
#'
#' @return Path to the directory holding `Project.toml`, `Manifest.toml` and
#'   the `EpiAwareR` Julia package.
#' @keywords internal
.julia_project <- function() {
  project <- system.file("julia", package = "EpiAwareR")
  if (!nzchar(project) || !file.exists(file.path(project, "Project.toml"))) {
    # Development load (pkgload::load_all) from the source tree.
    project <- file.path(getwd(), "inst", "julia")
  }
  project
}

#' Julia version the bundled Manifest was resolved with
#'
#' @param project Path to the bundled Julia project.
#' @return Character string with the major and minor version (e.g. `"1.12"`),
#'   or `NULL` if it cannot be read.
#' @keywords internal
.manifest_julia_version <- function(project = .julia_project()) {
  manifest <- file.path(project, "Manifest.toml")
  if (!file.exists(manifest)) {
    return(NULL)
  }
  line <- grep("^julia_version", readLines(manifest, warn = FALSE),
               value = TRUE)
  version <- regmatches(line, regexpr("[0-9]+\\.[0-9]+", line))
  if (length(version) == 0) NULL else version
}

#' Install a Julia version with juliaup
#'
#' @param version Character string with the Julia version (e.g. `"1.12"`).
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @return Path to the Julia executable, or `NULL` if juliaup is unavailable
#'   or the installation failed.
#' @keywords internal
.juliaup_julia <- function(version, verbose = TRUE) {
  if (!nzchar(Sys.which("juliaup"))) {
    if (verbose) {
      message(
        "juliaup not found, using the Julia on the PATH. Install juliaup ",
        "from https://github.com/JuliaLang/juliaup to match Julia ", version,
        "."
      )
    }
    return(NULL)
  }
  status <- tryCatch(
    system2("juliaup", c("add", version), stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  )
  if (!identical(as.integer(status), 0L)) {
    return(NULL)
  }
  julia_dirs <- list.dirs(
    file.path(path.expand("~"), ".julia", "juliaup"), recursive = FALSE
  )
  matching <- julia_dirs[startsWith(basename(julia_dirs),
                                    paste0("julia-", version, "."))]
  exe <- if (.Platform$OS.type == "windows") "julia.exe" else "julia"
  bins <- file.path(sort(matching, decreasing = TRUE), "bin", exe)
  bins <- bins[file.exists(bins)]
  if (length(bins) == 0) NULL else bins[1]
}

#' Set up Julia for EpiAwareR
#'
#' Installs the pinned Julia dependencies of EpiAwareR
#' (ComposableTuringIDModels.jl, Turing.jl and their dependencies) and starts
#' a Julia session with them loaded. This happens automatically on first use,
#' so it only needs calling directly to see progress or to troubleshoot.
#'
#' If [juliaup](https://github.com/JuliaLang/juliaup) is available, the Julia
#' version the dependencies were resolved with is installed and used.
#' Otherwise the Julia on the `PATH` (or in `JULIA_BINDIR`) is used, which
#' must be new enough for the bundled `Manifest.toml`. Set the
#' environment variable `JULIA_NUM_THREADS` before Julia starts to sample MCMC
#' chains in parallel.
#'
#' The first call can take several minutes while Julia packages are installed
#' and precompiled.
#'
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @return Invisibly `TRUE` on success.
#'
#' @examples
#' \dontrun{
#' epiaware_setup_julia()
#' }
#' @export
epiaware_setup_julia <- function(verbose = TRUE) {
  if (epiaware_available()) {
    return(invisible(TRUE))
  }
  project <- .julia_project()
  version <- .manifest_julia_version(project)
  if (!is.null(version) && !nzchar(Sys.getenv("JULIACONNECTOR_JULIABIN"))) {
    bin <- .juliaup_julia(version, verbose)
    if (!is.null(bin)) Sys.setenv(JULIACONNECTOR_JULIABIN = bin)
  }

  juliaready::julia_ready(
    packages = c("ComposableTuringIDModels", "Distributions",
                 "EpiAwareR"),
    state_env = .epiaware_env,
    project = project,
    verbose = verbose
  )

  if (verbose) message("EpiAwareR Julia backend ready")
  invisible(TRUE)
}

#' Check whether the Julia backend is running
#'
#' A Julia process that has gone away leaves the setup flag behind, so
#' clearing it here lets the next call set Julia up again rather than fail on
#' a session that no longer holds the bridge. Recovery works where
#' JuliaConnectoR has dropped the connection itself, after
#' [JuliaConnectoR::stopJulia()] or an interrupt; a process killed from
#' outside leaves a stale connection that `stopJulia()` clears.
#'
#' @return Logical. `TRUE` if Julia has been set up in this R session.
#'
#' @examples
#' epiaware_available()
#' @export
epiaware_available <- function() {
  if (!isTRUE(.epiaware_env$ready)) {
    return(FALSE)
  }
  running <- tryCatch(
    isTRUE(juliaready::eval_julia("isdefined(Main, :EpiAwareR)")),
    error = function(e) FALSE
  )
  if (!running) {
    # The same environment is kept, because handle ownership is its identity.
    .epiaware_env$ready <- FALSE
  }
  running
}

#' Call a bridge function, starting Julia if needed
#'
#' Releases the Julia objects of fits that R has garbage collected before
#' making the call. Releasing waits until here because finalisers may run
#' while another Julia call is in progress.
#'
#' @param fn Name of a function in the `EpiAwareR` Julia module.
#' @param ... Arguments passed to the function.
#' @return The translated result of the call.
#' @keywords internal
.bridge <- function(fn, ...) {
  if (!epiaware_available()) epiaware_setup_julia(verbose = FALSE)
  released <- .epiaware_env$released
  .epiaware_env$released <- NULL
  for (fit in released) {
    try(juliaready::call_julia("EpiAwareR.release!", fit$handle, fit$session),
        silent = TRUE)
  }
  juliaready::call_julia(paste0("EpiAwareR.", fn), ...)
}

#' Keep a Julia-side object alive while an R object refers to it
#'
#' The session token travels with the handle because handles are numbered
#' from the same base in every Julia session, so one from an earlier session
#' would otherwise name whichever fit now holds that number.
#'
#' Saving a fit copies the environment by value but not its finaliser, so a
#' reloaded fit names a Julia object it does not own. Recording this session's
#' state environment tells the two apart, because a copy of it is no longer
#' the same object.
#'
#' @param handle Integer handle returned by the bridge.
#' @param session Character token identifying the Julia session.
#' @return An environment holding the handle, released when collected.
#' @keywords internal
.julia_handle <- function(handle, session) {
  env <- new.env(parent = emptyenv())
  env$handle <- handle
  env$session <- session
  env$owner <- .epiaware_env
  reg.finalizer(env, function(e) {
    if (!identical(e$owner, .epiaware_env)) {
      return(invisible(NULL))
    }
    .epiaware_env$released <- c(
      .epiaware_env$released, list(list(handle = e$handle, session = e$session))
    )
  })
  env
}
