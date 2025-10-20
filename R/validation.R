# Internal validation helper functions

#' Check Julia/EpiAware availability
#' @keywords internal
.check_julia <- function() {
  if (!epiaware_available()) {
    stop(
      "Julia is not available. Please run:\n",
      "  epiaware_setup_julia()\n",
      "to install and configure Julia.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Validate distribution specification
#' @keywords internal
.check_distribution <- function(x, name = deparse(substitute(x)),
                                null_ok = FALSE) {
  if (is.null(x) && null_ok) {
    return(invisible(TRUE))
  }

  checkmate::assert_list(x, names = "named", .var.name = name)

  if (is.null(x$type) || is.null(x$params)) {
    stop(
      name, " must be a distribution specification.\n",
      "Use functions like norm(), halfnorm(), gamma_dist(), etc.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate list of distribution specifications
#' @keywords internal
.check_distribution_list <- function(x, name = deparse(substitute(x)),
                                     len = NULL) {
  checkmate::assert_list(x, .var.name = name)

  if (!is.null(len)) {
    checkmate::assert_int(len, lower = 1)
    if (length(x) != len) {
      stop("length of ", name, " must equal ", len, call. = FALSE)
    }
  }

  # Check each element is a distribution
  for (i in seq_along(x)) {
    .check_distribution(x[[i]], name = paste0(name, "[[", i, "]]"))
  }

  invisible(TRUE)
}

#' Validate model component class
#' @keywords internal
.check_model_component <- function(x, type, name = deparse(substitute(x))) {
  required_class <- switch(type,
    latent = "epiaware_latent",
    epi = "epiaware_epi",
    observation = "epiaware_observation",
    problem = "epiaware_problem",
    stop("Unknown component type: ", type, call. = FALSE)
  )

  # Accept either the specific class OR epiaware_generic (from epiaware_call)
  is_valid <- inherits(x, required_class) || inherits(x, "epiaware_generic")

  if (!is_valid) {
    type_name <- switch(type,
      latent = "latent model",
      epi = "infection model",
      observation = "observation model",
      problem = "EpiProblem"
    )

    stop(
      name, " must be ",
      if (type == "problem") "an " else "a ",
      type_name, " object",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Validate tspan
#' @keywords internal
.check_tspan <- function(tspan, name = "tspan") {
  checkmate::assert_numeric(
    tspan, len = 2, any.missing = FALSE, .var.name = name
  )

  if (tspan[1] >= tspan[2]) {
    stop(name, "[1] must be less than ", name, "[2]", call. = FALSE)
  }

  invisible(TRUE)
}
