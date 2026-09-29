#' Configure the NUTS sampler
#'
#' Settings for the No-U-Turn Sampler used by [fit()].
#'
#' Chains run in parallel only if Julia was started with more than one
#' thread; set the environment variable `JULIA_NUM_THREADS` (e.g. to `"4"`)
#' before Julia starts.
#'
#' @param draws Integer. Number of post-warmup draws per chain.
#' @param warmup Integer. Number of warmup (adaptation) iterations per chain,
#'   which are discarded.
#' @param chains Integer. Number of chains.
#' @param target_acceptance Numeric. Target acceptance rate during
#'   adaptation. Increase towards one if there are divergent transitions.
#' @param max_depth Integer. Maximum tree depth.
#' @param ad Character string. Automatic differentiation backend:
#'   `"forwarddiff"` works for every model and is fast for models with few
#'   parameters; `"mooncake"` scales better to long time series but compiles
#'   slowly on first use.
#'
#' @return An object of class `cidm_nuts`.
#'
#' @family inference
#' @examples
#' nuts(draws = 500, warmup = 500, chains = 2)
#' @export
nuts <- function(draws = 1000, warmup = 1000, chains = 4,
                 target_acceptance = 0.8, max_depth = 10,
                 ad = c("forwarddiff", "mooncake")) {
  checkmate::assert_count(draws, positive = TRUE)
  checkmate::assert_count(warmup)
  checkmate::assert_count(chains, positive = TRUE)
  checkmate::assert_number(target_acceptance, lower = 0, upper = 1)
  checkmate::assert_count(max_depth, positive = TRUE)
  structure(
    list(
      draws = as.integer(draws),
      warmup = as.integer(warmup),
      chains = as.integer(chains),
      target_acceptance = target_acceptance,
      max_depth = as.integer(max_depth),
      ad = match.arg(ad)
    ),
    class = "cidm_nuts"
  )
}

#' @export
print.cidm_nuts <- function(x, ...) {
  cat("<composableIDModelR NUTS sampler>\n")
  cat("  Chains:", x$chains, "\n")
  cat("  Warmup:", x$warmup, "per chain\n")
  cat("  Draws:", x$draws, "per chain\n")
  cat("  Target acceptance:", x$target_acceptance, "\n")
  cat("  Maximum tree depth:", x$max_depth, "\n")
  cat("  AD backend:", x$ad, "\n")
  invisible(x)
}

#' Fit a model to data
#'
#' Conditions a composed model on an observed time series and samples from the
#' posterior with NUTS.
#'
#' @param model A composed model from [IDModel()], or a [julia()] expression
#'   that builds one.
#' @param y Numeric vector of observations, one per time point. Missing
#'   observations are not supported, and neither is a matrix of stratified
#'   observations.
#' @param method Sampler settings from [nuts()].
#' @param dates Optional vector of dates (`Date` or `POSIXt`), one per
#'   observation, used when plotting.
#' @param seed Optional integer seed for the Julia random number generator.
#'
#' @return An object of class `cidm_fit` with elements
#' \describe{
#'   \item{draws}{Posterior draws of the model parameters, a
#'     [posterior::draws_df].}
#'   \item{sampler}{Sampler diagnostics per draw (e.g. `numerical_error` for
#'     divergent transitions), a [posterior::draws_df].}
#'   \item{generated}{Named list of draws x time matrices of generated
#'     quantities: `Z_t` (the latent process), `I_t` (infections),
#'     `expected_y_t` (expected observations), `predicted_y_t` (posterior
#'     predictive observations) and, for renewal models, `Rt`. It also holds
#'     `generated_y_t`, which is the observed series repeated for every draw,
#'     because the model was conditioned on it. In `expected_y_t` and
#'     `predicted_y_t`, time points the model does not score, such as those
#'     before a reporting delay has elapsed, are `NaN`.}
#'   \item{model, y, dates, method}{The inputs.}
#' }
#'
#' @family inference
#' @seealso [predict.cidm_fit()] to forecast, [plot.cidm_fit()] to
#'   visualise the fit.
#' @examples
#' \dontrun{
#' model <- IDModel(
#'   Renewal(generation_time = Gamma(shape = 6.5, scale = 0.62), rt = AR()),
#'   NegativeBinomialError(cluster_factor = HalfNormal(0.1))
#' )
#' y <- simulate(model, n = 40, seed = 1)$generated_y_t[1, ]
#' fitted <- fit(model, y, method = nuts(draws = 250, warmup = 250))
#' fitted
#' plot(fitted, type = "Rt")
#' }
#' @export
fit <- function(model, y, method = nuts(), dates = NULL, seed = NULL) {
  .assert_role(model, "model")
  checkmate::assert_numeric(y, min.len = 2, finite = TRUE)
  if (!is.null(dim(y))) {
    stop(
      "`y` must be a vector of observations. Stratified data, which ",
      "ComposableTuringIDModels.jl takes as a matrix, is not supported yet.",
      call. = FALSE
    )
  }
  if (anyNA(y)) {
    stop(
      "`y` contains missing values, which composableIDModelR cannot pass to Julia: ",
      "an `NA` becomes a number on the way, so it cannot be marked as ",
      "unobserved. Fit a window without gaps, and do not simply delete the ",
      "missing entries, which would move every later observation earlier in ",
      "time.",
      call. = FALSE
    )
  }
  if (!inherits(method, "cidm_nuts")) {
    stop("`method` must be a sampler from `nuts()`.", call. = FALSE)
  }
  checkmate::assert_multi_class(
    dates, c("Date", "POSIXct", "POSIXlt"), null.ok = TRUE
  )
  if (!is.null(dates) && length(dates) != length(y)) {
    stop("`dates` must have one entry per observation.", call. = FALSE)
  }
  checkmate::assert_int(seed, null.ok = TRUE)

  result <- .bridge(
    "fit", as_julia(model, ascii = TRUE), as.numeric(y),
    draws = method$draws, warmup = method$warmup, chains = method$chains,
    target_acceptance = method$target_acceptance,
    max_depth = method$max_depth, ad = method$ad,
    seed = if (!is.null(seed)) as.integer(seed)
  )

  index <- list(.chain = result$chain, .iteration = result$iteration)
  structure(
    list(
      draws = .as_draws(result$parameters, result$parameter_names, index),
      sampler = .as_draws(result$stats, result$stat_names, index),
      generated = .with_rt(.generated_list(result$generated), model),
      model = model,
      y = y,
      dates = dates,
      method = method,
      julia = .julia_handle(result$handle, result$session)
    ),
    class = "cidm_fit"
  )
}

#' Simulate from a model's prior
#'
#' Draws parameters from their priors and simulates the latent process,
#' infections and observations.
#'
#' @param object A composed model from [IDModel()], or a [julia()] expression
#'   that builds one.
#' @param nsim Integer. Number of simulations.
#' @param seed Optional integer seed for the Julia random number generator.
#' @param n Integer. Number of time points.
#' @param ... Unused.
#'
#' @return A named list of `nsim` x `n` matrices: `generated_y_t` (simulated
#'   observations), `expected_y_t`, `I_t`, `Z_t` and, for renewal models built
#'   with [Renewal()], `Rt`. A model written as [julia()] code gets no `Rt`,
#'   since its latent scale is not known to R. Time points without an
#'   observation (e.g. before a reporting delay has elapsed) are `NaN`.
#'
#' @family inference
#' @examples
#' \dontrun{
#' model <- IDModel(
#'   DirectInfections(Z = RandomWalk(), initialisation = Normal(log(100), 1)),
#'   PoissonError()
#' )
#' sims <- simulate(model, nsim = 10, n = 30, seed = 42)
#' matplot(t(sims$generated_y_t), type = "l")
#' }
#' @importFrom stats simulate
#' @export
simulate.cidm_model <- function(object, nsim = 1, seed = NULL, n, ...) {
  # An untyped `julia()` expression dispatches here too, as do typed ones of
  # every role, so the role is checked rather than left to dispatch.
  .assert_role(object, "model")
  checkmate::assert_count(nsim, positive = TRUE)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_int(seed, null.ok = TRUE)
  result <- .bridge(
    "simulate", as_julia(object, ascii = TRUE), as.integer(n),
    as.integer(nsim), if (!is.null(seed)) as.integer(seed)
  )
  .with_rt(.generated_list(result), object)
}

#' @rdname simulate.cidm_model
#' @export
simulate.cidm_julia <- simulate.cidm_model

#' Convert bridge output to a draws_df
#'
#' @param values Matrix of draws (rows) by variables (columns).
#' @param names Character vector of variable names.
#' @param index List with `.chain` and `.iteration` vectors.
#' @return A [posterior::draws_df].
#' @keywords internal
.as_draws <- function(values, names, index) {
  values <- matrix(values, nrow = length(index$.chain))
  colnames(values) <- unlist(names)
  draws <- as.data.frame(values, check.names = FALSE)
  draws$.chain <- as.integer(index$.chain)
  draws$.iteration <- as.integer(index$.iteration)
  posterior::as_draws_df(draws)
}

#' Convert generated quantities returned by the bridge to a named list
#'
#' @param generated List with `names` and a draws x time x quantities array
#'   of `values`.
#' @return Named list of draws x time matrices.
#' @keywords internal
.generated_list <- function(generated) {
  names <- unlist(generated$names)
  values <- generated$values
  dims <- dim(values)
  stats::setNames(
    lapply(seq_along(names), function(i) {
      matrix(values[, , i], nrow = dims[1], ncol = dims[2])
    }),
    names
  )
}

#' Add the reproduction number to generated quantities of renewal models
#'
#' With the default transformation, the latent process of a renewal model is
#' the log reproduction number.
#'
#' @param generated Named list of matrices.
#' @param model The composed model.
#' @return `generated`, with an `Rt` element where applicable. A model given
#'   as Julia code has no components to read, so it gets none.
#' @keywords internal
.with_rt <- function(generated, model) {
  if (inherits(model, "cidm_julia")) {
    return(generated)
  }
  infection <- model$args[[1]]
  if (identical(infection$fn, "Renewal") &&
        is.null(infection$kwargs$transformation) &&
        !is.null(generated$Z_t)) {
    generated$Rt <- exp(generated$Z_t)
  }
  generated
}
