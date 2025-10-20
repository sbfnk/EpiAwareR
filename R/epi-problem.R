# EpiProblem: Complete epidemiological model composition

#' Compose a Complete Epidemiological Model
#'
#' Creates an EpiProblem that composes infection, latent, and observation
#' models into a complete epidemiological model specification. The latent
#' model generates time-varying epidemiological parameters, the infection
#' model simulates disease transmission, and the observation model links
#' latent infections to observed data.
#'
#' @param epi_model An infection model object (e.g., from \code{Renewal}).
#' @param latent_model A latent process model (e.g., from \code{AR}).
#' @param observation_model An observation model (e.g., from
#'   \code{NegativeBinomialError}).
#' @param tspan A numeric vector of length 2 specifying the time span
#'   for inference or simulation: c(start_time, end_time).
#'
#' @return An S3 object of class \code{c("epiaware_problem", "epiaware_model")}
#'   containing:
#' \describe{
#'   \item{julia_ref}{Reference to the Julia EpiProblem object}
#'   \item{components}{List containing the three model components}
#'   \item{tspan}{The time span}
#' }
#'
#' @examples
#' \dontrun{
#' # Compose a complete epidemiological model (Mishra et al. 2020)
#' ar2 <- AR(
#'   order = 2,
#'   damp_priors = list(truncnorm(0.2, 0.2, 0, 1), truncnorm(0.1, 0.05, 0, 1)),
#'   init_priors = list(norm(0, 0.2), norm(0, 0.2)),
#'   std_prior = halfnorm(0.1)
#' )
#'
#' renewal <- Renewal(
#'   gen_distribution = gamma_dist(6.5, 0.62),
#'   initialisation_prior = norm(log(1.0), 0.1)
#' )
#'
#' negbin <- NegativeBinomialError(
#'   cluster_factor_prior = halfnorm(0.1)
#' )
#'
#' model <- EpiProblem(
#'   epi_model = renewal,
#'   latent_model = ar2,
#'   observation_model = negbin,
#'   tspan = c(45, 80)
#' )
#'
#' print(model)
#' }
#'
#' @export
# nolint start: object_name_linter.
EpiProblem <- function(epi_model, latent_model, observation_model, tspan) {
  # nolint end: object_name_linter.
  # Validate inputs
  .check_model_component(epi_model, type = "epi")
  .check_model_component(latent_model, type = "latent")
  .check_model_component(observation_model, type = "observation")
  .check_tspan(tspan)
  .check_julia()

  # Create Julia EpiProblem
  # EpiProblem(epi_model::E, latent_model::L, observation_model::O,
  #            tspan::Tuple{Int64, Int64})
  julia_tspan <- .to_julia_tuple(as.integer(tspan))
  julia_obj <- .call_julia_constructor(
    "EpiProblem",
    list(
      epi_model = epi_model$julia_ref,
      latent_model = latent_model$julia_ref,
      observation_model = observation_model$julia_ref,
      tspan = julia_tspan
    ),
    use_keywords = FALSE
  )

  # Return S3 object
  structure(
    list(
      julia_ref = julia_obj,
      components = list(
        epi_model = epi_model,
        latent_model = latent_model,
        observation_model = observation_model
      ),
      tspan = tspan
    ),
    class = c("epiaware_problem", "epiaware_model")
  )
}

#' Print method for EpiAware problems
#'
#' @param x An \code{epiaware_problem} object.
#' @param ... Additional arguments (currently unused).
#' @export
print.epiaware_problem <- function(x, ...) {
  cat("<EpiAware Epidemiological Model>\n")
  cat("  Time span:", x$tspan[1], "to", x$tspan[2], "\n")
  cat("  Components:\n")
  cat("    - Infection model:", class(x$components$epi_model)[1], "\n")
  cat("    - Latent model:", class(x$components$latent_model)[1], "\n")
  cat("    - Observation model:",
      class(x$components$observation_model)[1], "\n")
  invisible(x)
}
