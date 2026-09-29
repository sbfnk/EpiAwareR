# Observation models linking infections to observed data.

#' Observation models
#'
#' Models linking expected observations to data. Error models
#' (`PoissonError()`, `NegativeBinomialError()`, `NormalError()`) define the
#' likelihood; modifiers (`LatentDelay()`, `Ascertainment()`) wrap another
#' observation model to transform the expected observations first. Each wraps
#' the constructor of the same name in ComposableTuringIDModels.jl. `NULL`
#' arguments use the Julia default, and `delta_d` is the discretisation width
#' Julia spells with a Greek delta.
#'
#' @param cluster_factor Prior for the negative binomial cluster factor,
#'   \eqn{\sqrt{1/\phi}}, which is approximately the coefficient of variation
#'   of the observation noise.
#' @param std Prior for the standard deviation of normal observation error.
#' @param model The observation model to wrap.
#' @param delay The delay from infection to observation: a continuous
#'   distribution (discretised in Julia with double interval censoring), a
#'   numeric probability vector whose first entry is a delay of zero, or a
#'   `NonParametric()` or `Fixed()` distribution, which is discretised in R.
#'   Uncertain parameters give an inferred delay.
#' @param D Numeric. Maximum delay used when discretising a distribution,
#'   taken from the distribution's `max` when it has one.
#' @param delta_d Numeric. Discretisation interval width.
#' @param latent_model Ascertainment on the log scale: a distribution for a
#'   constant ascertainment or a latent model for a time-varying one.
#'
#' @return An object of class `epiaware_observation`.
#'
#' @family components
#' @name observation-models
#' @examples
#' # Negative binomial reporting of infections after an incubation period
#' LatentDelay(
#'   NegativeBinomialError(cluster_factor = HalfNormal(0.1)),
#'   delay = LogNormal(1.6, 0.42)
#' )
#'
#' # Poisson reporting of 10% of infections
#' Ascertainment(PoissonError(), latent_model = FixedIntercept(log(0.1)))
NULL

# nolint start: object_name_linter.

#' @rdname observation-models
#' @export
PoissonError <- function() {
  component("PoissonError", role = "observation")
}

#' @rdname observation-models
#' @export
NegativeBinomialError <- function(cluster_factor = NULL) {
  cluster_factor <- .as_prior_slot(cluster_factor)
  component(
    "NegativeBinomialError", cluster_factor = cluster_factor,
    role = "observation"
  )
}

#' @rdname observation-models
#' @export
NormalError <- function(std = NULL) {
  std <- .as_prior_slot(std)
  component("NormalError", std = std, role = "observation")
}

#' @rdname observation-models
#' @export
LatentDelay <- function(model, delay, D = NULL, delta_d = NULL) {
  .assert_role(model, "observation")
  .assert_positive(D, "D")
  .assert_positive(delta_d, "delta_d")
  delay <- .as_delay(delay, D, delta_d)
  max_delay <- delay$max_delay
  args <- list(model, delay$dist,
               D = if (!is.null(max_delay)) as.numeric(max_delay))
  args[["\u0394d"]] <- if (!is.null(delay$delta_d)) as.numeric(delay$delta_d)
  do.call(component, c("LatentDelay", args, role = "observation"))
}

#' @rdname observation-models
#' @export
Ascertainment <- function(model, latent_model) {
  .assert_role(model, "observation")
  latent_model <- .as_prior_slot(latent_model)
  if (is.null(latent_model)) {
    stop("`latent_model` must be a distribution or a latent model.",
         call. = FALSE)
  }
  component("Ascertainment", model, latent_model, role = "observation")
}

# nolint end
