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
  draws <- posterior::as_draws_matrix(fit$samples)
  vars <- colnames(draws)

  # Get epsilon columns and sort by time index
  eps_vars <- grep("latent\\..*_t\\.", vars, value = TRUE)
  if (length(eps_vars) == 0) {
    message("No latent process parameters found.")
    return(ggplot2::ggplot() + ggplot2::theme_minimal())
  }
  eps_idx <- as.integer(gsub(".*\\.(\\d+)\\.$", "\\1", eps_vars))
  eps_vars <- eps_vars[order(eps_idx)]
  n_time <- length(eps_vars)
  n_draws <- nrow(draws)

  # Get AR parameter columns
  damp_var <- grep("latent\\.damp_AR", vars, value = TRUE)[1]
  std_var <- grep("latent\\.std$", vars, value = TRUE)[1]
  init_var <- grep("latent\\.ar_init", vars, value = TRUE)[1]

  # Reconstruct latent AR process for each draw
  rt_matrix <- matrix(NA, n_draws, n_time)
  for (i in seq_len(n_draws)) {
    damp <- draws[i, damp_var]
    std <- draws[i, std_var]
    init <- draws[i, init_var]
    eps <- draws[i, eps_vars]

    latent <- numeric(n_time)
    latent[1] <- init + std * eps[1]
    for (t in 2:n_time) {
      latent[t] <- damp * latent[t - 1] + std * eps[t]
    }
    rt_matrix[i, ] <- exp(latent)
  }

  df <- data.frame(
    time = seq_len(n_time),
    median = apply(rt_matrix, 2, median),
    q5 = apply(rt_matrix, 2, quantile, 0.05),
    q95 = apply(rt_matrix, 2, quantile, 0.95)
  )

  ggplot2::ggplot(df, ggplot2::aes(x = time)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q5, ymax = q95),
                         fill = "steelblue", alpha = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = median), color = "steelblue") +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Rt", x = "Time", y = expression(R[t])) +
    ggplot2::theme_minimal()
}

#' Plot posterior predictive for cases
#' @keywords internal
.plot_posterior_predictive <- function(fit, ...) {
  # Get observed data
  if (is.null(fit$data)) {
    message("No data available for cases plot.")
    return(ggplot2::ggplot() + ggplot2::theme_minimal())
  }

  obs_data <- fit$data

  # Determine the case column name
  case_col <- intersect(c("confirm", "y_t", "cases", "count"), names(obs_data))
  if (length(case_col) == 0) {
    case_col <- names(obs_data)[2]  # Assume second column if no match
  } else {
    case_col <- case_col[1]
  }

  # Determine time/date column
  date_col <- intersect(c("date", "time", "t"), names(obs_data))
  if (length(date_col) > 0) {
    date_col <- date_col[1]
    obs_data$time_idx <- seq_len(nrow(obs_data))
    use_date <- inherits(obs_data[[date_col]], "Date")
  } else {
    obs_data$time_idx <- seq_len(nrow(obs_data))
    use_date <- FALSE
  }

  # Try to reconstruct expected cases from latent infections
  draws <- posterior::as_draws_matrix(fit$samples)
  vars <- colnames(draws)

  # Look for infection-related parameters
  inf_vars <- grep("epi\\.I_t\\.", vars, value = TRUE)

  if (length(inf_vars) > 0) {
    # Extract infection trajectories
    inf_idx <- as.integer(gsub(".*\\.(\\d+)\\.$", "\\1", inf_vars))
    inf_vars <- inf_vars[order(inf_idx)]
    n_time <- min(length(inf_vars), nrow(obs_data))

    inf_matrix <- as.matrix(draws[, inf_vars[seq_len(n_time)]])

    pred_df <- data.frame(
      time_idx = seq_len(n_time),
      median = apply(inf_matrix, 2, median),
      q5 = apply(inf_matrix, 2, quantile, 0.05),
      q95 = apply(inf_matrix, 2, quantile, 0.95)
    )

    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(
        data = pred_df,
        ggplot2::aes(x = time_idx, ymin = q5, ymax = q95),
        fill = "steelblue", alpha = 0.3
      ) +
      ggplot2::geom_line(
        data = pred_df,
        ggplot2::aes(x = time_idx, y = median),
        color = "steelblue"
      ) +
      ggplot2::geom_point(
        data = obs_data[seq_len(n_time), ],
        ggplot2::aes(x = time_idx, y = .data[[case_col]]),
        color = "black", size = 2
      ) +
      ggplot2::labs(
        title = "Observed vs Fitted Cases",
        subtitle = "Points: observed, Line: posterior median, Ribbon: 90% CI",
        x = "Time",
        y = "Cases"
      ) +
      ggplot2::theme_minimal()

    return(p)
  }

  # Fallback: just plot observed data
  p <- ggplot2::ggplot(obs_data, ggplot2::aes(x = time_idx, y = .data[[case_col]])) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Observed Cases",
      subtitle = "Predicted cases not available",
      x = "Time",
      y = "Cases"
    ) +
    ggplot2::theme_minimal()

  return(p)
}

#' Plot posterior distributions for parameters
#' @keywords internal
.plot_posterior_distributions <- function(fit, ...) {

  if (!requireNamespace("bayesplot", quietly = TRUE)) {
    message("Package 'bayesplot' recommended for posterior plots.")
    message("Install it with: install.packages('bayesplot')")

    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Posterior Distributions") +
        ggplot2::theme_minimal()
    )
  }

  draws <- posterior::as_draws_matrix(fit$samples)
  vars <- setdiff(colnames(draws), c(".chain", ".iteration", ".draw"))

  # Filter to key parameters only (exclude time-indexed parameters like _t.1., _t.2., etc.)
  # Keep: damp_AR, std, ar_init, cluster_factor, init (scalar parameters)
  key_patterns <- c("damp_AR", "^latent\\.std$", "ar_init", "cluster_factor",
                    "^epi\\.init$", "initialisation")
  key_vars <- vars[grepl(paste(key_patterns, collapse = "|"), vars)]

  # If no key vars found, fall back to non-time-indexed parameters

  if (length(key_vars) == 0) {
    key_vars <- vars[!grepl("_t\\.\\d+\\.", vars)]
  }

  # Filter to finite values only
  finite_vars <- key_vars[vapply(key_vars, function(v) {
    all(is.finite(draws[, v]))
  }, logical(1))]


  if (length(finite_vars) == 0) {
    message("No finite parameters found for posterior plot.")
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "Posterior Distributions") +
        ggplot2::theme_minimal()
    )
  }

  bayesplot::mcmc_areas(draws[, finite_vars, drop = FALSE])
}
