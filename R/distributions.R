# Distribution constructor functions for EpiAwareR
# These create specification lists that are converted to Julia
# Distributions.jl objects

#' Normal distribution
#'
#' Specifies a normal (Gaussian) distribution.
#'
#' @param mean Numeric. Mean of the distribution.
#' @param sd Numeric. Standard deviation of the distribution.
#'
#' @return A list specification for a normal distribution.
#'
#' @examples
#' \dontrun{
#' # Standard normal
#' prior1 <- norm(0, 1)
#'
#' # Prior for log Rt
#' prior2 <- norm(log(1.5), 0.2)
#' }
#'
#' @export
norm <- function(mean, sd) {
  checkmate::assert_number(mean, finite = TRUE)
  checkmate::assert_number(sd, lower = 0, finite = TRUE)
  list(type = "Normal", params = c(mean, sd))
}

#' Truncated normal distribution
#'
#' Specifies a normal distribution truncated to a specified range.
#'
#' @param mean Numeric. Mean of the underlying normal distribution.
#' @param sd Numeric. Standard deviation of the underlying normal distribution.
#' @param lower Numeric. Lower truncation bound.
#' @param upper Numeric. Upper truncation bound.
#'
#' @return A list specification for a truncated normal distribution.
#'
#' @examples
#' \dontrun{
#' # AR damping coefficient bounded to [0, 1]
#' damp_prior <- truncnorm(0.8, 0.2, 0, 1)
#' }
#'
#' @export
truncnorm <- function(mean, sd, lower, upper) {
  checkmate::assert_number(mean, finite = TRUE)
  checkmate::assert_number(sd, lower = 0, finite = TRUE)
  checkmate::assert_number(lower, finite = TRUE)
  checkmate::assert_number(upper, finite = TRUE)
  checkmate::assert_true(lower < upper)

  list(
    type = "truncated(Normal(mean, sd), lower, upper)",
    params = c(mean = mean, sd = sd, lower = lower, upper = upper)
  )
}

#' Half-normal distribution
#'
#' Specifies a half-normal distribution (normal distribution truncated to
#' positive values).
#'
#' @param sd Numeric. Scale parameter (standard deviation of underlying normal).
#'
#' @return A list specification for a half-normal distribution.
#'
#' @examples
#' \dontrun{
#' # Prior for standard deviation parameters
#' sigma_prior <- halfnorm(0.1)
#' }
#'
#' @export
halfnorm <- function(sd) {
  checkmate::assert_number(sd, lower = 0, finite = TRUE)
  list(type = "truncated(Normal(0, sd), 0, Inf)", params = c(sd = sd))
}

#' Gamma distribution
#'
#' Specifies a gamma distribution using shape and scale parameterization.
#'
#' @param shape Numeric. Shape parameter (alpha).
#' @param scale Numeric. Scale parameter (theta). Note: this is scale, not rate.
#'
#' @return A list specification for a gamma distribution.
#'
#' @examples
#' \dontrun{
#' # Generation time distribution with mean 6.5 and scale 0.62
#' gen_time <- gamma_dist(6.5, 0.62)
#' }
#'
#' @export
gamma_dist <- function(shape, scale) {
  checkmate::assert_number(shape, lower = 0, finite = TRUE)
  checkmate::assert_number(scale, lower = 0, finite = TRUE)
  list(type = "Gamma", params = c(shape, scale))
}

#' Log-normal distribution
#'
#' Specifies a log-normal distribution.
#'
#' @param meanlog Numeric. Mean of the distribution on the log scale.
#' @param sdlog Numeric. Standard deviation of the distribution on the log
#'   scale.
#'
#' @return A list specification for a log-normal distribution.
#'
#' @examples
#' \dontrun{
#' # Delay distribution
#' delay <- lognorm(1.6, 0.42)
#' }
#'
#' @export
lognorm <- function(meanlog, sdlog) {
  checkmate::assert_number(meanlog, finite = TRUE)
  checkmate::assert_number(sdlog, lower = 0, finite = TRUE)
  list(type = "LogNormal", params = c(meanlog, sdlog))
}

#' Exponential distribution
#'
#' Specifies an exponential distribution.
#'
#' @param rate Numeric. Rate parameter (inverse of mean).
#'
#' @return A list specification for an exponential distribution.
#'
#' @examples
#' \dontrun{
#' # Exponential prior with mean 10
#' prior <- exponential(1 / 10)
#' }
#'
#' @export
exponential <- function(rate) {
  checkmate::assert_number(rate, lower = 0, finite = TRUE)
  list(type = "Exponential", params = c(rate))
}
