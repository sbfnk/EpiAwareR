
# EpiAwareR

<!-- badges: start -->

<!-- badges: end -->

**EpiAwareR** is an R interface to the Julia-based
[EpiAware](https://github.com/CDCgov/Rt-without-renewal) compositional
infectious disease modelling framework. It enables R users to build
flexible epidemiological models by composing reusable components without
requiring Julia expertise.

## Installation

### Prerequisites

EpiAwareR requires:

- R \>= 4.0.0
- Julia \>= 1.9.0 (automatically installed if not present)

### Install from GitHub

``` r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install EpiAwareR
devtools::install_github("CDCgov/Rt-without-renewal", subdir = "EpiAwareR")
```

### Setup

On first use, EpiAwareR will attempt to automatically install Julia and
required packages. If this fails, run:

``` r
library(EpiAwareR)
epiaware_setup_julia()
```

## Quick Start

Here’s a basic example replicating Mishra et al. (2020) analysis of
COVID-19 in South Korea:

``` r
library(EpiAwareR)

# Load data
training_data <- read.csv("https://raw.githubusercontent.com/EpiAware/PrototypeCompositionalProbablisticInfectiousDiseaseModelling/refs/heads/main/data/south_korea_data.csv")
colnames(training_data)[3] <- "cases"

# Define latent process: AR(2) for log Rt
ar2 <- AR(
  order = 2,
  damp_priors = list(
    truncnorm(0.2, 0.2, 0, 1),
    truncnorm(0.1, 0.05, 0, 1)
  ),
  init_priors = list(norm(0, 0.2), norm(0, 0.2)),
  std_prior = halfnorm(0.1)
)

# Define infection model: Renewal process
renewal <- Renewal(
  gen_distribution = gamma_dist(6.5, 0.62),
  initialisation_prior = norm(log(1.0), 0.1)
)

# Define observation model: Negative binomial
negbin <- NegativeBinomialError(
  cluster_factor_prior = halfnorm(0.1)
)

# Compose into complete model
model <- EpiProblem(
  epi_model = renewal,
  latent_model = ar2,
  observation_model = negbin,
  tspan = c(45, 80)
)

# Fit model
results <- fit(
  model,
  data = training_data,
  method = nuts_sampler(warmup = 1000, draws = 1000, chains = 4)
)

# Examine results
print(results)
summary(results)
plot(results, type = "Rt")
plot(results, type = "cases")
```

## Development Status

EpiAwareR is currently in MVP (Minimum Viable Product) phase,
implementing core components needed for the Mishra et al. (2020) case
study.

## Contributing

Contributions welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for
guidelines.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->

<!-- prettier-ignore-start -->

<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [all-contributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

<a href="https://github.com/sbfnk/EpiAwareR/commits?author=sbfnk">sbfnk</a>

<!-- markdownlint-enable -->

<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
