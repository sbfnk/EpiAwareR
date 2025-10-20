# EpiAwareR (development version)

## EpiAwareR 0.1.0 (MVP Release)

Initial MVP release of EpiAwareR providing R interface to Julia-based EpiAware framework.

### Features

#### Core Components

* **Latent Models**
  - `AR()`: Autoregressive processes of arbitrary order for time-varying parameters
  - Generic access to other models (MA, ARMA, ARIMA) via `epiaware_call()`

* **Infection Models**
  - `Renewal()`: Renewal equation with customizable generation time distribution
  - Support for fixed and uncertain generation time distributions

* **Observation Models**
  - `NegativeBinomialError()`: Overdispersed count observations
  - `LatentDelay()`: Reporting delay wrapper enabling hierarchical composition

* **Model Composition**
  - `EpiProblem()`: Compose latent, infection, and observation models into complete epidemiological model
  - Automatic validation of component compatibility

#### Inference

* **Bayesian Inference**
  - `fit()`: Fit models to data using MCMC sampling
  - `nuts_sampler()`: Configure NUTS (No-U-Turn Sampler) for efficient inference
  - Integration with Turing.jl PPL backend

* **Results and Diagnostics**
  - S3 methods: `print()`, `summary()`, `plot()`
  - Convergence diagnostics (Rhat, ESS)
  - Posterior predictive distributions
  - Generated quantities (Rt trajectories, infections, predictions)

#### Distributions

* Prior specification functions:
  - `norm()`: Normal distribution
  - `truncnorm()`: Truncated normal
  - `halfnorm()`: Half-normal
  - `gamma_dist()`: Gamma distribution
  - `lognorm()`: Log-normal distribution
  - `exponential()`: Exponential distribution

#### Extensibility

* **Tiered Access Pattern**
  - Tier 1: Explicit R wrappers for common components (AR, Renewal, NegativeBinomialError)
  - Tier 2: Generic `epiaware_call()` for accessing newer Julia features
  - Tier 3: Direct JuliaCall access for power users

#### Setup and Configuration

* `epiaware_setup_julia()`: Automated Julia installation and configuration
* `epiaware_available()`: Check Julia/EpiAware availability
* Graceful degradation when Julia unavailable

### Documentation

* Comprehensive README with quick start example
* Getting Started vignette with Mishra et al. (2020) case study
* Complete function reference with roxygen2 documentation
* Contributing guidelines

### Testing

* Unit tests for all components using testthat
* Integration tests for end-to-end workflows
* Tests skip gracefully when Julia unavailable

### Case Studies

* Mishra et al. (2020): COVID-19 in South Korea
  - AR(2) latent process
  - Renewal infection model
  - Negative binomial observations
  - Demonstrates compositional approach

### Known Limitations

* Plotting methods currently return placeholder plots
* Generated quantities extraction needs full implementation
* Limited to NUTS inference (other samplers available via `epiaware_call()`)
* Some advanced EpiAware features only accessible via generic wrapper

### Future Plans

* Enhanced plotting with actual Rt and case trajectory visualization
* Additional explicit wrappers for MA, ARMA, ARIMA models
* Day-of-week effect observation models
* Additional case studies and vignettes
* Integration with other R epidemiological packages
* CRAN submission
