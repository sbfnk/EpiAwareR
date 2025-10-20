# Observation process models linking latent infections to observed data

#' Negative Binomial Observation Error Model
#'
#' Links latent infections to observed case counts using a negative binomial
#' distribution. The cluster_factor parameterizes the overdispersion as the
#' coefficient of variation (sqrt(1/phi)), which is more intuitive for setting
#' priors than the dispersion parameter phi directly.
#'
#' @param cluster_factor_prior Distribution specification for the cluster factor
#'   (sqrt(1/phi)), which represents the coefficient of variation of observation
#'   noise.
#'
#' @return An S3 object of class \code{c("epiaware_negbin",
#'   "epiaware_observation", "epiaware_model")} containing:
#' \describe{
#'   \item{julia_ref}{Reference to the Julia NegativeBinomialError object}
#'   \item{spec}{List of model specifications}
#' }
#'
#' @examples
#' \dontrun{
#' # Negative binomial observation model
#' negbin <- NegativeBinomialError(
#'   cluster_factor_prior = halfnorm(0.1)
#' )
#' print(negbin)
#' }
#'
#' @export
# nolint start: object_name_linter.
NegativeBinomialError <- function(cluster_factor_prior) {
  # nolint end: object_name_linter.
  # Validate inputs
  .check_distribution(cluster_factor_prior)
  .check_julia()

  # Convert prior to Julia
  julia_prior <- .to_julia_dist(cluster_factor_prior)

  # Create NegativeBinomialError model using positional constructor
  julia_obj <- .call_julia_constructor(
    "NegativeBinomialError",
    list(cluster_factor_prior = julia_prior),
    use_keywords = FALSE
  )

  # Return S3 object
  structure(
    list(
      julia_ref = julia_obj,
      spec = list(cluster_factor_prior = cluster_factor_prior)
    ),
    class = c("epiaware_negbin", "epiaware_observation", "epiaware_model")
  )
}

#' Latent Delay Observation Model
#'
#' Wraps an observation model with a reporting delay, convolving the latent
#' infections with a delay distribution. This is used to model delays between
#' infection and observation (e.g., incubation period, reporting delays).
#'
#' @param model An observation model object (e.g., from
#'   \code{NegativeBinomialError}).
#' @param delay_distribution Distribution specification for the delay.
#'   Can be any continuous distribution that will be discretized.
#'
#' @return An S3 object of class \code{c("epiaware_delay",
#'   "epiaware_observation", "epiaware_model")} containing:
#' \describe{
#'   \item{julia_ref}{Reference to the Julia LatentDelay object}
#'   \item{base_model}{The wrapped observation model}
#'   \item{spec}{List of model specifications}
#' }
#'
#' @examples
#' \dontrun{
#' # Add incubation and reporting delays to observation model
#' negbin <- NegativeBinomialError(halfnorm(0.1))
#'
#' # Add incubation delay
#' incubation_model <- LatentDelay(
#'   negbin,
#'   delay_distribution = lognorm(1.6, 0.42)
#' )
#'
#' # Add reporting delay
#' full_model <- LatentDelay(
#'   incubation_model,
#'   delay_distribution = lognorm(0.58, 0.47)
#' )
#' }
#'
#' @export
# nolint start: object_name_linter.
LatentDelay <- function(model, delay_distribution) {
  # nolint end: object_name_linter.
  # Validate inputs
  .check_model_component(model, type = "observation")
  .check_distribution(delay_distribution)
  .check_julia()

  # Convert delay distribution to Julia
  julia_delay <- .to_julia_dist(delay_distribution)

  # Create LatentDelay model wrapping the base model
  # LatentDelay(model::M, distribution::C; D, Δd)
  julia_obj <- .call_julia_constructor(
    "LatentDelay",
    list(model = model$julia_ref, distribution = julia_delay),
    use_keywords = FALSE
  )

  # Return S3 object
  structure(
    list(
      julia_ref = julia_obj,
      base_model = model,
      spec = list(delay_distribution = delay_distribution)
    ),
    class = c("epiaware_delay", "epiaware_observation", "epiaware_model")
  )
}

#' Print method for negative binomial observation models
#'
#' @param x An \code{epiaware_negbin} object.
#' @param ... Additional arguments (currently unused).
#' @export
print.epiaware_negbin <- function(x, ...) {
  cat("<EpiAware Negative Binomial Observation Model>\n")
  cat("  Cluster factor prior:", x$spec$cluster_factor_prior$type, "\n")
  invisible(x)
}

#' Print method for latent delay observation models
#'
#' @param x An \code{epiaware_delay} object.
#' @param ... Additional arguments (currently unused).
#' @export
print.epiaware_delay <- function(x, ...) {
  cat("<EpiAware Latent Delay Observation Model>\n")
  cat("  Delay distribution:", x$spec$delay_distribution$type, "\n")
  cat("  Base model:", class(x$base_model)[1], "\n")
  invisible(x)
}
