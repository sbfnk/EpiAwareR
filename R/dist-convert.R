# Conversion of distspec distributions to Julia components.
#
# distspec and Distributions.jl differ in how they parameterise some
# distributions, so each is mapped explicitly. A distribution used as a prior
# is converted to a Julia distribution; one used as a delay or generation time
# may instead become a probability vector, or a component whose parameters are
# inferred.

# Julia constructor and parameter conversion per distspec distribution. Each
# entry gives the Julia name and a function from the distspec natural
# parameters to the positional arguments of that constructor.
.julia_distributions <- list(
  normal = list(fn = "Normal", args = function(p) list(p$mean, p$sd)),
  lognormal = list(
    fn = "LogNormal", args = function(p) list(p$meanlog, p$sdlog)
  ),
  gamma = list(fn = "Gamma", args = function(p) list(p$shape, 1 / p$rate)),
  exp = list(fn = "Exponential", args = function(p) list(1 / p$rate)),
  weibull = list(fn = "Weibull", args = function(p) list(p$shape, p$scale)),
  beta = list(fn = "Beta", args = function(p) list(p$shape1, p$shape2))
)

# Distributions whose distspec parameters are also the positional arguments of
# the Julia constructor, and which can therefore be given priors.
.inferrable_distributions <- c("normal", "lognormal", "weibull", "beta")

#' Convert a distribution used as a prior to a Julia component
#'
#' Priors are sampled by the model, so their parameters must be fixed.
#'
#' @param x A distspec distribution or an EpiAwareR component.
#' @param arg_name Name used in error messages.
#' @return A component of class `epiaware_distribution`.
#' @keywords internal
.as_prior <- function(x, arg_name = deparse(substitute(x))) {
  if (!inherits(x, "dist_spec")) {
    .assert_role(x, "distribution", arg_name = arg_name)
    return(x)
  }
  .assert_single(x, arg_name)
  distribution <- distspec::get_distribution(x)
  if (distspec::has_uncertainty(x)) {
    stop(
      "`", arg_name, "` is used as a prior, so its parameters must be fixed. ",
      "Uncertain parameters are only supported for delays and generation ",
      "times.",
      call. = FALSE
    )
  }
  if (!distribution %in% names(.julia_distributions)) {
    stop(
      "`", arg_name, "` is a ", distribution, " distribution, which cannot be ",
      "used as a prior. Use a parametric distribution, or `component()` for a ",
      "Distributions.jl distribution with no distspec equivalent.",
      call. = FALSE
    )
  }
  spec <- .julia_distributions[[distribution]]
  args <- lapply(spec$args(distspec::get_parameters(x)), as.numeric)
  prior <- do.call(
    component, c(spec$fn, args, role = "distribution")
  )
  max <- .dist_max(x)
  if (is.finite(max)) {
    prior <- component("truncated", prior, -Inf, max, role = "distribution")
  }
  prior
}

#' Convert a distribution used as a delay or generation time
#'
#' Delays and generation times are discretised, so a distribution may also be
#' given as a probability vector, as a point mass, or with priors on its
#' parameters.
#'
#' @param x A distspec distribution, an EpiAwareR component, or a numeric
#'   probability vector.
#' @param max_delay Numeric. Maximum value used when discretising, or `NULL`
#'   to take it from the distribution's `max`.
#' @param delta_d Numeric. Discretisation interval width.
#' @param drop_zero Logical. Whether to drop the zero delay from a probability
#'   vector and renormalise, as a generation time requires.
#' @param arg_name Name used in error messages.
#' @return A list with the converted `dist` (a component or a list of
#'   probabilities) and the `max_delay` and `delta_d` still to be passed on,
#'   which are `NULL` once the conversion has used them.
#' @keywords internal
.as_delay <- function(x, max_delay = NULL, delta_d = NULL, drop_zero = FALSE,
                      arg_name = deparse(substitute(x))) {
  if (is.numeric(x)) {
    return(list(
      dist = .as_pmf(x, arg_name), max_delay = max_delay, delta_d = delta_d
    ))
  }
  if (!inherits(x, "dist_spec")) {
    .assert_role(x, "distribution", arg_name = arg_name)
    return(list(
      dist = x, max_delay = max_delay, delta_d = delta_d
    ))
  }
  .assert_single(x, arg_name)

  distribution <- distspec::get_distribution(x)
  max <- .dist_max(x)
  if (is.null(max_delay) && is.finite(max)) max_delay <- max

  if (distribution %in% c("nonparametric", "fixed")) {
    pmf <- distspec::get_pmf(distspec::discretise(x))
    if (drop_zero) pmf <- pmf[-1] / sum(pmf[-1])
    return(list(
      dist = .as_pmf(pmf, arg_name), max_delay = NULL, delta_d = NULL
    ))
  }
  if (!distribution %in% names(.julia_distributions)) {
    stop(
      "`", arg_name, "` is a ", distribution, " distribution, which cannot be ",
      "used as a delay.",
      call. = FALSE
    )
  }
  if (!distspec::has_uncertainty(x)) {
    return(list(
      dist = .as_prior(.unbound(x), arg_name),
      max_delay = max_delay, delta_d = delta_d
    ))
  }

  # Uncertain parameters become priors on the arguments of the Julia
  # constructor, which the model samples and rediscretises per draw.
  if (!distribution %in% .inferrable_distributions) {
    stop(
      "`", arg_name, "` has uncertain parameters, which EpiAwareR supports ",
      "for ", paste(.inferrable_distributions, collapse = ", "),
      " distributions only. A ", distribution, " distribution is parameterised",
      " differently in Julia, so a prior on its parameters cannot be carried ",
      "over.",
      call. = FALSE
    )
  }
  if (is.null(max_delay) || !is.finite(max_delay)) {
    stop(
      "`", arg_name, "` has uncertain parameters, so it needs a finite ",
      "maximum to discretise it, e.g. `max = 15`.",
      call. = FALSE
    )
  }
  parameters <- distspec::get_parameters(x)
  fixed <- !vapply(parameters, inherits, logical(1), "dist_spec")
  if (any(fixed)) {
    stop(
      "`", arg_name, "` mixes fixed and uncertain parameters, which ",
      "ComposableTuringIDModels.jl cannot infer. Give every parameter a ",
      "prior; for a nearly fixed ", names(parameters)[fixed][1], ", use a ",
      "narrow one.",
      call. = FALSE
    )
  }
  # Priors are truncated at the lower bounds of the parameters they describe,
  # e.g. zero for a standard deviation, so that no draw is invalid.
  bounds <- distspec::lower_bounds(x)
  priors <- lapply(names(parameters), function(name) {
    prior <- .as_prior(parameters[[name]], arg_name)
    if (is.finite(bounds[[name]])) {
      prior <- component(
        "truncated", prior, as.numeric(bounds[[name]]), Inf,
        role = "distribution"
      )
    }
    prior
  })
  args <- list(julia(.julia_distributions[[distribution]]$fn),
               unname(priors), D = as.numeric(max_delay))
  args[["\u0394d"]] <- if (!is.null(delta_d)) as.numeric(delta_d)
  list(
    dist = do.call(component, c("UncertainDelay", args, role = "distribution")),
    max_delay = NULL, delta_d = NULL
  )
}

#' Check that a distribution is a single distribution
#'
#' @param x A distspec distribution.
#' @param arg_name Name used in error messages.
#' @return Invisibly `TRUE`.
#' @keywords internal
.assert_single <- function(x, arg_name) {
  if (distspec::ndist(x) > 1) {
    stop(
      "`", arg_name, "` combines ", distspec::ndist(x), " distributions. ",
      "Convolve them first with `distspec::collapse(distspec::discretise(x))`.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Maximum of a distspec distribution
#'
#' @param x A distspec distribution.
#' @return The maximum, or `Inf` if it is unbounded.
#' @keywords internal
.dist_max <- function(x) {
  max <- attr(x, "max")
  if (is.null(max)) Inf else max
}

#' Drop the bounds of a distspec distribution
#'
#' A delay distribution's maximum is passed to Julia as the discretisation
#' horizon, so the distribution itself is converted unbounded.
#'
#' @param x A distspec distribution.
#' @return `x` without its `max` attribute.
#' @keywords internal
.unbound <- function(x) {
  attr(x, "max") <- NULL
  x
}

#' Convert a prior slot, which may hold several distributions
#'
#' Prior slots accept a distribution, a list of them (one per lag), or a
#' latent model for a time-varying parameter.
#'
#' @param x The slot value, or `NULL` to use the Julia default.
#' @param arg_name Name used in error messages.
#' @return The converted slot value.
#' @keywords internal
.as_prior_slot <- function(x, arg_name = deparse(substitute(x))) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.list(x) && !inherits(x, c("epiaware_component", "dist_spec"))) {
    return(lapply(seq_along(x), function(i) {
      .as_prior(x[[i]], paste0(arg_name, "[[", i, "]]"))
    }))
  }
  if (inherits(x, "dist_spec")) {
    return(.as_prior(x, arg_name))
  }
  .assert_role(x, c("distribution", "latent"), arg_name = arg_name)
  x
}

#' Validate a probability mass function
#'
#' @param x Numeric vector.
#' @param arg_name Name used in error messages.
#' @return `x` as a list of doubles, so a length-one PMF still renders as a
#'   Julia vector.
#' @keywords internal
.as_pmf <- function(x, arg_name = deparse(substitute(x))) {
  checkmate::assert_numeric(
    x, lower = 0, min.len = 1, any.missing = FALSE, .var.name = arg_name
  )
  if (abs(sum(x) - 1) > 1e-8) {
    stop("A numeric `", arg_name, "` must sum to one.", call. = FALSE)
  }
  as.list(as.numeric(x))
}
