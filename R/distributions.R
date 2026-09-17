# Distributions come from distspec, which EpiAwareR converts to Julia code.
# Only the two distributions with no distspec equivalent are defined here.

#' Probability distributions
#'
#' Distributions are specified with
#' [distspec](https://epiforecasts.io/distspec/), whose constructors
#' `Normal()`, `Gamma()`, `LogNormal()`, `Exponential()`, `Weibull()`,
#' `Beta()`, `Fixed()` and `NonParametric()` EpiAwareR re-exports. Parameters
#' can be given naturally (`Gamma(shape = 2, rate = 0.5)`) or as a mean and
#' standard deviation (`Gamma(mean = 4, sd = 2)`).
#'
#' EpiAwareR adds two constructors of its own:
#'
#' - `HalfNormal()`, the prior from ComposableTuringIDModels.jl, parameterised
#'   by its mean.
#' - `truncated()`, which bounds a distribution below as well as above. The
#'   `max` argument of a distspec distribution bounds it above only.
#'
#' Where a distribution is used decides what it may contain. A prior, such as
#' the damping of an [AR()] process, needs fixed parameters. A delay or
#' generation time may have uncertain parameters (for example
#' `LogNormal(meanlog = Normal(1.6, 0.2), sdlog = 0.4, max = 15)`), and is
#' then inferred along with the rest of the model; such a distribution needs a
#' finite `max` to bound its support.
#'
#' @param mean Numeric. Mean of the half-normal distribution.
#' @param dist A distribution to truncate.
#' @param lower,upper Numeric truncation bounds. Use `-Inf` or `Inf` to leave
#'   a side unbounded.
#'
#' @return A component of class `epiaware_distribution`.
#'
#' @family components
#' @name distributions
#' @examples
#' # A prior bounded to [0, 1]
#' truncated(Normal(0.8, 0.05), 0, 1)
#'
#' # A generation time, discretised in Julia
#' Gamma(shape = 6.5, scale = 0.62)
#'
#' # A delay whose parameters are inferred
#' LogNormal(meanlog = Normal(1.6, 0.2), sdlog = 0.4, max = 15)
NULL

#' @rdname distributions
#' @export
# nolint start: object_name_linter.
HalfNormal <- function(mean = 1) {
  checkmate::assert_number(mean, lower = 0, finite = TRUE)
  component("HalfNormal", as.numeric(mean), role = "distribution")
}
# nolint end

#' @rdname distributions
#' @export
truncated <- function(dist, lower = -Inf, upper = Inf) {
  dist <- .as_prior(dist)
  checkmate::assert_number(lower)
  checkmate::assert_number(upper)
  if (lower >= upper) {
    stop("`lower` must be less than `upper`.", call. = FALSE)
  }
  component("truncated", dist, as.numeric(lower), as.numeric(upper),
            role = "distribution")
}

#' @importFrom distspec Normal
#' @export
distspec::Normal

#' @importFrom distspec Gamma
#' @export
distspec::Gamma

#' @importFrom distspec LogNormal
#' @export
distspec::LogNormal

#' @importFrom distspec Exponential
#' @export
distspec::Exponential

#' @importFrom distspec Weibull
#' @export
distspec::Weibull

#' @importFrom distspec Beta
#' @export
distspec::Beta

#' @importFrom distspec Fixed
#' @export
distspec::Fixed

#' @importFrom distspec NonParametric
#' @export
distspec::NonParametric
