# Infection models, each owning the latent process that drives it.

#' Infection models
#'
#' Models for the unobserved infection process. Each carries its own latent
#' process: the log reproduction number for `Renewal()`, the log growth rate
#' for `ExpGrowthRate()`, and log infections for `DirectInfections()`. Each
#' wraps the constructor of the same name in ComposableTuringIDModels.jl.
#' `NULL` arguments use the Julia default.
#'
#' @param generation_time The generation interval: a continuous distribution
#'   (discretised in Julia with double interval censoring), or a numeric
#'   probability vector whose first entry is a delay of one day. A
#'   `NonParametric()` or `Fixed()` distribution is discretised in R, and its
#'   zero-day mass is dropped and the rest renormalised, as Julia does when it
#'   discretises a continuous distribution. Uncertain parameters give an
#'   inferred generation interval.
#' @param rt Latent model for the log reproduction number (`Renewal()`) or the
#'   growth rate (`ExpGrowthRate()`). A distribution gives a constant value.
#' @param Z Latent model for log infections.
#' @param initialisation Prior for initial infections on the log scale.
#' @param transformation A [julia()] function mapping the latent scale to
#'   infections.
#' @param D_gen Numeric. Maximum generation interval used when discretising a
#'   distribution, taken from the distribution's `max` when it has one.
#' @param delta_d Numeric. Discretisation interval width.
#'
#' @return An object of class `epiaware_infection`.
#'
#' @family components
#' @name infection-models
#' @examples
#' Renewal(
#'   generation_time = Gamma(shape = 6.5, scale = 0.62),
#'   rt = AR(),
#'   initialisation = Normal(log(1), 0.1)
#' )
#'
#' DirectInfections(Z = RandomWalk(), initialisation = Normal(log(100), 1))
NULL

# nolint start: object_name_linter.

#' @rdname infection-models
#' @export
Renewal <- function(generation_time, rt = NULL, initialisation = NULL,
                    transformation = NULL, D_gen = NULL, delta_d = NULL) {
  checkmate::assert_number(D_gen, lower = 0, null.ok = TRUE)
  checkmate::assert_number(delta_d, lower = 0, null.ok = TRUE)
  gen <- .as_delay(generation_time, D_gen, delta_d, drop_zero = TRUE,
                   max_name = "D_gen")
  rt <- .as_prior_slot(rt)
  initialisation <- .as_prior_slot(initialisation)
  .assert_role(transformation, "julia", null_ok = TRUE)
  args <- list(
    generation_time = gen$dist, rt = rt,
    initialisation = initialisation, transformation = transformation,
    D_gen = if (!is.null(gen$max_delay)) as.numeric(gen$max_delay)
  )
  args[["\u0394d"]] <- if (!is.null(gen$delta_d)) as.numeric(gen$delta_d)
  do.call(component, c("Renewal", args, role = "infection"))
}

#' @rdname infection-models
#' @export
DirectInfections <- function(Z = NULL, initialisation = NULL,
                             transformation = NULL) {
  Z <- .as_prior_slot(Z)
  initialisation <- .as_prior_slot(initialisation)
  .assert_role(transformation, "julia", null_ok = TRUE)
  component(
    "DirectInfections", Z = Z, initialisation = initialisation,
    transformation = transformation, role = "infection"
  )
}

#' @rdname infection-models
#' @export
ExpGrowthRate <- function(rt = NULL, initialisation = NULL,
                          transformation = NULL) {
  rt <- .as_prior_slot(rt)
  initialisation <- .as_prior_slot(initialisation)
  .assert_role(transformation, "julia", null_ok = TRUE)
  component(
    "ExpGrowthRate", rt = rt, initialisation = initialisation,
    transformation = transformation, role = "infection"
  )
}

# nolint end
