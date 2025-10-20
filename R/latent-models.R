# Latent process models for time-varying parameters

#' Autoregressive Process for Latent Dynamics
#'
#' Constructs an autoregressive (AR) latent process of order p for modeling
#' time-varying log reproduction numbers or other epidemiological parameters.
#'
#' @param order Integer. The order of the autoregressive process (p).
#' @param damp_priors List of distribution specifications for damping
#'   coefficients. Length must equal \code{order}.
#' @param init_priors List of distribution specifications for initial
#'   states. Length must equal \code{order}.
#' @param std_prior Distribution specification for innovation standard
#'   deviation.
#'
#' @return An S3 object of class \code{c("epiaware_ar", "epiaware_latent",
#'   "epiaware_model")} containing:
#' \describe{
#'   \item{julia_ref}{Reference to the Julia AR object}
#'   \item{spec}{List of model specifications}
#' }
#'
#' @examples
#' \dontrun{
#' # AR(2) model with truncated normal priors (Mishra et al. 2020)
#' ar2 <- AR(
#'   order = 2,
#'   damp_priors = list(
#'     truncnorm(0.2, 0.2, 0, 1),
#'     truncnorm(0.1, 0.05, 0, 1)
#'   ),
#'   init_priors = list(norm(0, 0.2), norm(0, 0.2)),
#'   std_prior = halfnorm(0.1)
#' )
#' print(ar2)
#' }
#'
#' @seealso \code{\link{epiaware_call}} for accessing other latent models
#' @export
# nolint start: object_name_linter.
AR <- function(order = 1, damp_priors, init_priors, std_prior) {
  # nolint end: object_name_linter.
  # Validate inputs
  checkmate::assert_int(order, lower = 1)
  .check_distribution_list(damp_priors, len = order)
  .check_distribution_list(init_priors, len = order)
  .check_distribution(std_prior)
  .check_julia()

  # Convert distributions to Julia vectors
  julia_damp <- .to_julia_dist_vector(damp_priors)
  julia_init <- .to_julia_dist_vector(init_priors)
  julia_std <- .to_julia_dist(std_prior)

  # Create HierarchicalNormal for error term using simple constructor
  julia_epsilon <- .call_julia_constructor(
    "HierarchicalNormal",
    list(std_prior = julia_std),
    use_keywords = FALSE
  )

  # Create Julia AR object using keyword constructor
  # AR(; damp_priors, init_priors, \u03f5_t)
  julia_obj <- .call_julia_constructor(
    "AR",
    list(
      damp_priors = julia_damp,
      init_priors = julia_init,
      "\u03f5_t" = julia_epsilon
    )
  )

  # Return S3 object
  structure(
    list(
      julia_ref = julia_obj,
      spec = list(
        order = order,
        damp_priors = damp_priors,
        init_priors = init_priors,
        std_prior = std_prior
      )
    ),
    class = c("epiaware_ar", "epiaware_latent", "epiaware_model")
  )
}

#' Print method for AR latent models
#'
#' @param x An \code{epiaware_ar} object.
#' @param ... Additional arguments (currently unused).
#' @export
print.epiaware_ar <- function(x, ...) {
  cat("<EpiAware AR(", x$spec$order, ") Latent Model>\n", sep = "")
  cat("  Damping priors:", length(x$spec$damp_priors), "\n")
  cat("  Init priors:", length(x$spec$init_priors), "\n")
  cat("  Innovation std prior: specified\n")
  invisible(x)
}
