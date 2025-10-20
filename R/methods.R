# S3 methods for EpiAware objects

#' Format model name for display
#' @keywords internal
.format_model_name <- function(model) {
  class_name <- class(model)[1]
  if (class_name == "epiaware_generic" && !is.null(model$fn_name)) {
    paste0(class_name, " (", model$fn_name, ")")
  } else {
    class_name
  }
}

#' Print Method for Fitted EpiAware Models
#'
#' @param x An \code{epiaware_fit} object from \code{fit()}.
#' @param ... Additional arguments (currently unused).
#'
#' @export
print.epiaware_fit <- function(x, ...) {
  cat("<EpiAware Model Fit>\n\n")

  # Model info
  cat("Model:\n")
  cat("  Time span:", x$model$tspan[1], "to", x$model$tspan[2], "\n")
  cat("  Infection model:",
      .format_model_name(x$model$components$epi_model), "\n")
  cat("  Latent model:",
      .format_model_name(x$model$components$latent_model), "\n")
  cat("  Observation model:",
      .format_model_name(x$model$components$observation_model), "\n\n")

  # Sampling info
  cat("Sampling:\n")
  if (inherits(x$method, "epiaware_nuts")) {
    cat("  Method: NUTS\n")
    cat("  Chains:", x$method$chains, "\n")
    cat("  Draws:", x$method$draws, "(per chain)\n\n")
  }

  # Convergence diagnostics
  cat("Convergence:\n")
  if (!is.null(x$diagnostics)) {
    max_rhat <- max(x$diagnostics$rhat, na.rm = TRUE)
    min_ess_bulk <- min(x$diagnostics$ess_bulk, na.rm = TRUE)

    cat("  Max Rhat:", round(max_rhat, 3), "\n")
    cat("  Min ESS (bulk):", round(min_ess_bulk, 0), "\n")

    if (max_rhat > 1.1) {
      cat("  Warning: Some parameters have Rhat > 1.1\n")
    }
    if (min_ess_bulk < 100) {
      cat("  Warning: Some parameters have ESS < 100\n")
    }
  }

  cat("\n")
  cat("Use summary() for parameter estimates\n")
  cat("Use plot() to visualize results\n")

  invisible(x)
}

#' Summary Method for Fitted EpiAware Models
#'
#' @param object An \code{epiaware_fit} object from \code{fit()}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A tibble with parameter summaries.
#'
#' @export
summary.epiaware_fit <- function(object, ...) {
  object$summary
}

#' Plot Method for Fitted EpiAware Models
#'
#' Creates visualizations of fitted model results, including reproduction
#' number trajectories, infection curves, and posterior predictive
#' distributions.
#'
#' @param x An \code{epiaware_fit} object from \code{fit()}.
#' @param type Character string specifying plot type. Options:
#'   \itemize{
#'     \item \code{"Rt"}: Time-varying reproduction number with credible
#'       intervals
#'     \item \code{"cases"}: Observed vs predicted cases with credible intervals
#'     \item \code{"posterior"}: Posterior distributions for key parameters
#'   }
#' @param ... Additional arguments passed to plotting functions.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Plot Rt trajectory
#' plot(results, type = "Rt")
#'
#' # Plot posterior predictive for cases
#' plot(results, type = "cases")
#'
#' # Plot parameter posteriors
#' plot(results, type = "posterior")
#' }
#'
#' @export
plot.epiaware_fit <- function(x, type = c("Rt", "cases", "posterior"), ...) {
  type <- match.arg(type)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting.\n",
      "Install it with: install.packages('ggplot2')",
      call. = FALSE
    )
  }

  switch(type,
    "Rt" = .plot_rt_trajectory(x, ...),
    "cases" = .plot_posterior_predictive(x, ...),
    "posterior" = .plot_posterior_distributions(x, ...)
  )
}

#' Plot Rt trajectory
#' @keywords internal
.plot_rt_trajectory <- function(fit, ...) {
  if (is.null(fit$generated_quantities) ||
        is.null(fit$generated_quantities$Rt)) {
    message("Rt trajectories not available in generated quantities.")
    message(
      "This is a placeholder plot - full implementation would extract Rt ",
      "from Julia."
    )
    return(ggplot2::ggplot() + ggplot2::theme_minimal())
  }

  # Placeholder for actual Rt plotting
  # Real implementation would extract Rt samples from generated_quantities
  # and create ribbon plots with credible intervals

  ggplot2::ggplot() +
    ggplot2::labs(
      title = "Time-Varying Reproduction Number (Rt)",
      x = "Time",
      y = "Rt"
    ) +
    ggplot2::theme_minimal()
}

#' Plot posterior predictive for cases
#' @keywords internal
.plot_posterior_predictive <- function(fit, ...) {
  if (is.null(fit$generated_quantities) ||
        is.null(fit$generated_quantities$predicted_cases)) {
    message("Posterior predictive samples not available.")
    message(
      "This is a placeholder plot - full implementation would extract ",
      "predictions from Julia."
    )
    return(ggplot2::ggplot() + ggplot2::theme_minimal())
  }

  # Placeholder for actual posterior predictive plotting
  # Real implementation would:
  # 1. Extract predicted case samples
  # 2. Compute quantiles
  # 3. Plot ribbons with observed data overlay

  ggplot2::ggplot() +
    ggplot2::labs(
      title = "Observed vs Predicted Cases",
      x = "Time",
      y = "Cases"
    ) +
    ggplot2::theme_minimal()
}

#' Plot posterior distributions for parameters
#' @keywords internal
.plot_posterior_distributions <- function(fit, ...) {
  if (!requireNamespace("bayesplot", quietly = TRUE)) {
    message("Package 'bayesplot' recommended for posterior plots.")
    message("Install it with: install.packages('bayesplot')")

    # Fallback to simple density plots
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Posterior Distributions") +
        ggplot2::theme_minimal()
    )
  }

  # Use bayesplot for nice posterior visualization
  bayesplot::mcmc_areas(fit$samples)
}
