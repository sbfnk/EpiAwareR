
# composableIDModelR

<!-- badges: start -->

<!-- badges: end -->

**composableIDModelR** is an R interface to
[ComposableTuringIDModels.jl](https://github.com/EpiAware/ComposableTuringIDModels.jl),
the composable probabilistic infectious disease modelling package of the
[EpiAware](https://epiaware.org) ecosystem. Models are assembled in R
from interchangeable latent process, infection and observation
components, then simulated from and fitted in Julia with
[Turing.jl](https://turinglang.org). No Julia code is needed, but every
model can be shown as the Julia code it runs.

## Installation

composableIDModelR needs [Julia](https://julialang.org) (\>= 1.12), the version
its pinned dependencies were resolved with. We recommend installing it
with [juliaup](https://github.com/JuliaLang/juliaup), which lets
composableIDModelR use the Julia version its dependencies were resolved with.

``` r
# install.packages("remotes")
remotes::install_github("sbfnk/composableIDModelR")
```

Julia packages are installed on first use. To watch progress, run:

``` r
library(composableIDModelR)
cidm_setup_julia()
```

## Quick start

Estimate the reproduction number from COVID-19 cases in South Korea,
following Mishra et al. (2020): an AR(2) process on log $R_t$ drives a
renewal model, observed with negative binomial noise.

``` r
library(composableIDModelR)

cases <- read.csv(system.file("extdata", "south_korea_data.csv",
                              package = "composableIDModelR"))[45:80, ]

renewal <- Renewal(
  generation_time = Gamma(shape = 6.5, scale = 0.62),
  rt = AR(
    damp = list(truncated(Normal(0.8, 0.05), 0, 1),
                truncated(Normal(0.1, 0.05), 0, 1)),
    init = list(Normal(0, 0.2), Normal(0, 0.2)),
    epsilon_t = HierarchicalNormal(std = HalfNormal(0.1))
  ),
  initialisation = Normal(log(1), 0.1)
)
model <- IDModel(
  renewal,
  NegativeBinomialError(cluster_factor = HalfNormal(0.1))
)

fitted <- fit(model, cases$cases_new, dates = as.Date(cases$date))
fitted
summary(fitted)
plot(fitted, type = "Rt")
plot(fitted, type = "cases", horizon = 14)
```

Swapping an assumption means swapping a component, for example Poisson
observations after a reporting delay:

``` r
delayed <- IDModel(
  renewal,
  LatentDelay(PoissonError(), delay = LogNormal(1.6, 0.42))
)
```

Constructors without an R wrapper are available through `component()`
and `julia()`.

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
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<a href="https://github.com/sbfnk/composableIDModelR/commits?author=sbfnk">sbfnk</a>

### Issues

<a href="https://github.com/sbfnk/composableIDModelR/issues?q=is%3Aissue+author%3Aseabbs">seabbs</a>,
<a href="https://github.com/sbfnk/composableIDModelR/issues?q=is%3Aissue+author%3Aowenjonesuob">owenjonesuob</a>

<!-- markdownlint-enable -->

<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->
