# Latent process models, generating time-varying parameters such as log Rt.
#
# Prior slots accept a distribution, a list of distributions (one per lag), or
# a latent model (giving a time-varying parameter), mirroring the Julia
# constructors. Innovation slots, written with a Greek epsilon in Julia, are
# `epsilon_t` here.

.epsilon_t <- "\u03f5_t"

#' Latent process models
#'
#' Models for the latent paths that drive infection models, such as the log
#' reproduction number in [Renewal()]. Each wraps the constructor of the same
#' name in ComposableTuringIDModels.jl; see its
#' [documentation](https://composableturingidmodels.epiaware.org/stable/) for
#' the full model definitions. `NULL` arguments use the Julia default.
#'
#' Arguments that Julia spells with a Greek letter are spelled out here:
#' `epsilon_t` is its innovation slot and `theta` its moving-average
#' coefficients.
#'
#' - `AR()`: autoregressive process whose order is the number of damping
#'   priors.
#' - `MA()`: moving-average process whose order is the number of coefficient
#'   priors.
#' - `RandomWalk()`: random walk.
#' - `IID()`: independent draws from a distribution.
#' - `Intercept()`: a single draw repeated over time.
#' - `FixedIntercept()`: a fixed value repeated over time.
#' - `HierarchicalNormal()`: non-centred normal draws with an inferred standard
#'   deviation, the default innovation model.
#' - `DiffLatentModel()`: a process whose differences follow `model`, giving
#'   e.g. ARIMA-style processes when `model` is an `AR()`.
#'
#' @param damp Prior for the damping coefficients: a distribution, a list of
#'   distributions (one per lag) or, for order one, a latent model for a
#'   time-varying coefficient. The two are on different scales: a distribution
#'   is a prior on the coefficient itself, while a latent model is squashed
#'   through `tanh` to keep the process stationary.
#' @param init Prior for the initial values: a distribution or a list of
#'   distributions (one per lag or difference).
#' @param epsilon_t Model for the innovations, typically a
#'   `HierarchicalNormal()`.
#' @param theta Prior for the moving-average coefficients, as for `damp`.
#' @param dist A distribution.
#' @param value Numeric. The fixed value.
#' @param mean Numeric. Mean of the process.
#' @param std Prior for the standard deviation.
#' @param model A latent model for the differenced process.
#'
#' @return An object of class `epiaware_latent`.
#'
#' @family components
#' @name latent-models
#' @examples
#' # AR(2) on log Rt, as in Mishra et al. (2020)
#' AR(
#'   damp = list(truncated(Normal(0.8, 0.05), 0, 1),
#'               truncated(Normal(0.1, 0.05), 0, 1)),
#'   init = list(Normal(0, 0.2), Normal(0, 0.2)),
#'   epsilon_t = HierarchicalNormal(std = HalfNormal(0.1))
#' )
#'
#' RandomWalk(init = Normal(0, 0.25))
NULL

# nolint start: object_name_linter.

#' @rdname latent-models
#' @export
AR <- function(damp = NULL, init = NULL, epsilon_t = NULL) {
  damp <- .as_prior_slot(damp)
  init <- .as_prior_slot(init)
  epsilon_t <- .as_prior_slot(epsilon_t)
  args <- list(damp = damp, init = init)
  args[[.epsilon_t]] <- epsilon_t
  do.call(component, c("AR", args, role = "latent"))
}

#' @rdname latent-models
#' @export
MA <- function(theta = NULL, epsilon_t = NULL) {
  theta <- .as_prior_slot(theta)
  epsilon_t <- .as_prior_slot(epsilon_t)
  args <- list()
  args[["\u03b8"]] <- theta
  args[[.epsilon_t]] <- epsilon_t
  do.call(component, c("MA", args, role = "latent"))
}

#' @rdname latent-models
#' @export
RandomWalk <- function(init = NULL, epsilon_t = NULL) {
  init <- .as_prior_slot(init)
  epsilon_t <- .as_prior_slot(epsilon_t)
  args <- list(init = init)
  args[[.epsilon_t]] <- epsilon_t
  do.call(component, c("RandomWalk", args, role = "latent"))
}

#' @rdname latent-models
#' @export
IID <- function(dist = Normal(0, 1)) {
  dist <- .as_prior(dist)
  component("IID", dist, role = "latent")
}

#' @rdname latent-models
#' @export
Intercept <- function(dist) {
  dist <- .as_prior(dist)
  component("Intercept", dist, role = "latent")
}

#' @rdname latent-models
#' @export
FixedIntercept <- function(value) {
  checkmate::assert_number(value, finite = TRUE)
  component("FixedIntercept", as.numeric(value), role = "latent")
}

#' @rdname latent-models
#' @export
HierarchicalNormal <- function(mean = NULL, std = NULL) {
  if (!is.null(mean)) {
    checkmate::assert_number(mean, finite = TRUE)
    mean <- as.numeric(mean)
  }
  std <- .as_prior_slot(std)
  component("HierarchicalNormal", mean = mean, std = std, role = "latent")
}

#' @rdname latent-models
#' @export
DiffLatentModel <- function(model, init = NULL) {
  model <- .as_prior_slot(model)
  init <- .as_prior_slot(init)
  if (inherits(init, "epiaware_distribution")) init <- list(init)
  component("DiffLatentModel", model = model, init = init, role = "latent")
}

# nolint end
