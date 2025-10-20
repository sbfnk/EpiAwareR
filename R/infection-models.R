# Infection generation process models

#' Renewal Process Infection Model
#'
#' Constructs a renewal process model for infections, where new infections
#' arise from previous infections weighted by a generation time distribution.
#' This implements the renewal equation I_t = R_t * sum(g_s * I_\{t-s\}).
#'
#' @param gen_distribution Distribution specification for the generation time
#'   (or serial interval). Can be any continuous distribution that will be
#'   discretized using double interval censoring.
#' @param initialisation_prior Optional distribution specification for the
#'   initial infection level (on log scale). If NULL, uses a default prior.
#'
#' @return An S3 object of class \code{c("epiaware_renewal", "epiaware_epi",
#'   "epiaware_model")} containing:
#' \describe{
#'   \item{julia_ref}{Reference to the Julia Renewal object}
#'   \item{spec}{List of model specifications}
#' }
#'
#' @examples
#' \dontrun{
#' # Renewal model with Gamma generation time (Mishra et al. 2020)
#' renewal <- Renewal(
#'   gen_distribution = gamma_dist(6.5, 0.62),
#'   initialisation_prior = norm(log(1.0), 0.1)
#' )
#' print(renewal)
#'
#' # For advanced features, use the generic wrapper
#' # to access newer EpiAware infection models
#' custom_model <- epiaware_call("NewInfectionModel", param1 = ...)
#' }
#'
#' @seealso \code{\link{epiaware_call}} for accessing other infection models
#' @export
# nolint start: object_name_linter.
Renewal <- function(gen_distribution, initialisation_prior = NULL) {
  # nolint end: object_name_linter.
  # Validate inputs
  .check_distribution(gen_distribution)
  .check_distribution(initialisation_prior, null_ok = TRUE)
  .check_julia()

  # Convert generation distribution to Julia
  julia_gen_dist <- .to_julia_dist(gen_distribution)

  # Create EpiData with generation distribution Use keyword constructor:
  # EpiData(; gen_distribution, D_gen, Δd, transformation)
  julia_epi_data <- tryCatch(
    {
      JuliaCall::julia_assign("gen_dist_tmp", julia_gen_dist)
      .eval_julia_code(
        "EpiData(gen_distribution=gen_dist_tmp, transformation=exp)"
      )
    },
    error = function(e) {
      stop("Failed to create EpiData:\n", conditionMessage(e), call. = FALSE)
    }
  )

  # Convert initialisation prior if provided
  julia_init <- if (!is.null(initialisation_prior)) {
    .to_julia_dist(initialisation_prior)
  } else {
    # Default to Normal(0, 1) if not provided
    .eval_julia_code("Normal(0, 1)")
  }

  # Create Renewal model using keyword constructor
  # Renewal(; data, initialisation_prior)
  julia_obj <- .call_julia_constructor(
    "Renewal",
    list(data = julia_epi_data, initialisation_prior = julia_init)
  )

  # Return S3 object
  structure(
    list(
      julia_ref = julia_obj,
      spec = list(
        gen_distribution = gen_distribution,
        initialisation_prior = initialisation_prior
      )
    ),
    class = c("epiaware_renewal", "epiaware_epi", "epiaware_model")
  )
}

#' Print method for renewal infection models
#'
#' @param x An \code{epiaware_renewal} object.
#' @param ... Additional arguments (currently unused).
#' @export
print.epiaware_renewal <- function(x, ...) {
  cat("<EpiAware Renewal Infection Model>\n")
  cat("  Generation distribution:", x$spec$gen_distribution$type, "\n")
  if (!is.null(x$spec$initialisation_prior)) {
    cat("  Initialisation prior: specified\n")
  } else {
    cat("  Initialisation prior: default\n")
  }
  invisible(x)
}
