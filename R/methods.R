#' @export
print.cidm_fit <- function(x, ...) {
  cat("<composableIDModelR fit>\n")
  cat("Model:\n")
  cat(paste0("  ", .format_code(x$model, width = 76L)), sep = "\n")
  cat("\nData:", length(x$y), "time points\n")
  cat("Sampling:", x$method$chains, "chains of", x$method$draws,
      "draws after", x$method$warmup, "warmup\n")

  diagnostics <- posterior::summarise_draws(
    x$draws, "rhat", "ess_bulk"
  )
  divergent <- posterior::extract_variable(x$sampler, "numerical_error")
  cat("\nConvergence:\n")
  cat("  Max Rhat:", format(max(diagnostics$rhat, na.rm = TRUE), digits = 3),
      "\n")
  cat("  Min bulk ESS:", round(min(diagnostics$ess_bulk, na.rm = TRUE)), "\n")
  cat("  Divergent transitions:", sum(divergent), "\n")
  cat("\nUse summary() for parameter estimates and plot() to visualise.\n")
  invisible(x)
}

#' Summarise the posterior of a fitted model
#'
#' @param object An `cidm_fit` object from [fit()].
#' @param ... Passed to [posterior::summarise_draws()].
#'
#' @return A `draws_summary` tibble with one row per parameter.
#'
#' @family inference
#' @export
summary.cidm_fit <- function(object, ...) {
  posterior::summarise_draws(object$draws, ...)
}

#' Posterior predictions and forecasts
#'
#' Draws observations from the posterior predictive distribution over the
#' fitted period and, if `horizon` is positive, forecasts beyond it. The
#' forecast extends the latent process with fresh innovations for each
#' posterior draw.
#'
#' Forecasting uses the Julia session in which the model was fitted.
#'
#' @param object An `cidm_fit` object from [fit()].
#' @param horizon Integer. Number of time points to forecast.
#' @param seed Optional integer seed for the Julia random number generator,
#'   used for the forecast. In-sample predictions are drawn when fitting.
#' @param ... Unused.
#'
#' @return A matrix with one row per posterior draw and one column per time
#'   point, covering the fitted period followed by the forecast horizon.
#'
#' @family inference
#' @examples
#' \dontrun{
#' forecasts <- predict(fitted, horizon = 14)
#' }
#' @importFrom stats predict
#' @export
predict.cidm_fit <- function(object, horizon = 0, seed = NULL, ...) {
  checkmate::assert_count(horizon)
  checkmate::assert_int(seed, null.ok = TRUE)
  in_sample <- object$generated$predicted_y_t
  if (horizon == 0) {
    return(in_sample)
  }
  if (!identical(object$julia$owner, .cidm_env)) {
    stop(
      "This fit was loaded from disk, and forecasting needs the Julia ",
      "session it was fitted in. Refit the model to forecast from it; the ",
      "draws and generated quantities it holds are still usable.",
      call. = FALSE
    )
  }
  forecasts <- tryCatch(
    .bridge(
      "forecast_observations", object$julia$handle, as.integer(horizon),
      if (!is.null(seed)) as.integer(seed), object$julia$session
    ),
    error = function(e) {
      stop(
        "Forecasting failed. If Julia has been restarted since fitting, ",
        "refit the model.\n", conditionMessage(e),
        call. = FALSE
      )
    }
  )
  cbind(in_sample, matrix(forecasts, nrow = nrow(in_sample)))
}

#' @importFrom posterior as_draws_df
#' @export
as_draws_df.cidm_fit <- function(x, ...) {
  posterior::as_draws_df(x$draws, ...)
}

#' @importFrom posterior as_draws_array
#' @export
as_draws_array.cidm_fit <- function(x, ...) {
  posterior::as_draws_array(x$draws, ...)
}

#' @importFrom posterior as_draws_matrix
#' @export
as_draws_matrix.cidm_fit <- function(x, ...) {
  posterior::as_draws_matrix(x$draws, ...)
}

#' @importFrom posterior as_draws_list
#' @export
as_draws_list.cidm_fit <- function(x, ...) {
  posterior::as_draws_list(x$draws, ...)
}

#' @importFrom posterior as_draws_rvars
#' @export
as_draws_rvars.cidm_fit <- function(x, ...) {
  posterior::as_draws_rvars(x$draws, ...)
}

#' Plot a fitted model
#'
#' Shows the posterior median and 50% and 90% credible intervals of a
#' generated quantity over time.
#'
#' @param x An `cidm_fit` object from [fit()].
#' @param type Character string. What to plot: `"cases"` (posterior
#'   predictive observations with the data), `"Rt"` (reproduction number,
#'   renewal models only), `"infections"`, or `"latent"` (the latent process
#'   on its own scale).
#' @param horizon Integer. For `type = "cases"`, number of time points to
#'   forecast.
#' @param ... Unused.
#'
#' @return A ggplot2 object.
#'
#' @family inference
#' @examples
#' \dontrun{
#' plot(fitted, type = "Rt")
#' plot(fitted, type = "cases", horizon = 14)
#' }
#' @export
plot.cidm_fit <- function(x, type = c("cases", "Rt", "infections",
                                          "latent"),
                              horizon = 0, ...) {
  type <- match.arg(type)
  quantity <- switch(type,
    cases = predict(x, horizon = horizon),
    Rt = x$generated$Rt,
    infections = x$generated$I_t,
    latent = x$generated$Z_t
  )
  if (is.null(quantity)) {
    stop(
      "No ", type, " trajectories are available for this model.",
      if (type == "Rt") paste(
        " `Rt` needs a `Renewal()` infection model on its default scale, so",
        "it is not derived when `transformation` is given."
      ),
      call. = FALSE
    )
  }
  labels <- c(
    cases = "Observations", Rt = "Reproduction number",
    infections = "Infections", latent = "Latent process"
  )
  bands <- .trajectory_bands(quantity, .time_axis(x, ncol(quantity)))
  p <- ggplot2::ggplot(bands, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q5, ymax = .data$q95),
      fill = "steelblue", alpha = 0.25
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$q25, ymax = .data$q75),
      fill = "steelblue", alpha = 0.4
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$median), colour = "steelblue4") +
    ggplot2::labs(x = if (is.null(x$dates)) "Time" else "Date",
                  y = labels[[type]]) +
    ggplot2::theme_minimal()
  if (type == "Rt") {
    p <- p + ggplot2::geom_hline(yintercept = 1, linetype = "dashed")
  }
  if (type == "cases") {
    observed <- data.frame(
      time = .time_axis(x, length(x$y)), y = x$y
    )
    p <- p + ggplot2::geom_point(
      data = observed[!is.na(observed$y), ],
      ggplot2::aes(x = .data$time, y = .data$y),
      inherit.aes = FALSE, size = 1
    )
    if (horizon > 0) {
      p <- p + ggplot2::geom_vline(
        xintercept = observed$time[nrow(observed)], linetype = "dotted"
      )
    }
  }
  p
}

#' Time axis for plotting
#'
#' @param fit An `cidm_fit` object.
#' @param n Number of time points, which may extend beyond the data.
#' @return Dates if the fit has them, otherwise integers.
#' @keywords internal
.time_axis <- function(fit, n) {
  if (is.null(fit$dates)) {
    return(seq_len(n))
  }
  # Kept as given, so that sub-daily observations keep their time of day.
  dates <- fit$dates
  n_dates <- length(dates)
  if (n <= n_dates) {
    return(dates[seq_len(n)])
  }
  # Forecasts continue at the spacing of the last two observations.
  step <- if (n_dates > 1) dates[n_dates] - dates[n_dates - 1] else 1
  c(dates, dates[n_dates] + step * seq_len(n - n_dates))
}

#' Summarise trajectories by time point
#'
#' @param trajectories Matrix of draws (rows) by time points (columns).
#' @param time Vector of time values, one per column.
#' @return A data frame with columns `time`, `median`, `q5`, `q25`, `q75`
#'   and `q95`, omitting time points without values.
#' @keywords internal
.trajectory_bands <- function(trajectories, time) {
  probs <- c(q5 = 0.05, q25 = 0.25, median = 0.5, q75 = 0.75, q95 = 0.95)
  trajectories[is.nan(trajectories)] <- NA
  quantiles <- t(apply(trajectories, 2, stats::quantile, probs = probs,
                       na.rm = TRUE, names = FALSE))
  colnames(quantiles) <- names(probs)
  bands <- data.frame(time = time, quantiles)
  bands[stats::complete.cases(bands), ]
}
