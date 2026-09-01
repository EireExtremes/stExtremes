
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stExtremes

Tools for spatio-temporal modelling of sea-level extremes with INLA.

<!-- badges: start -->

<!-- badges: end -->

stExtremes provides the pieces needed to fit Bayesian spatio-temporal
extreme-value models to sea-level data with the integrated nested
Laplace approximation:

- **Distributions.** Density, distribution, quantile, random generation,
  negative log-likelihood and return-level functions for the generalised
  extreme value (GEV) distribution and its blended variant (bGEV), in
  both the classical `(mu, sigma, xi)` and the quantile-based
  `(q, s, xi)` parametrisations.
- **INLA/SPDE helpers.** Mesh and polygon utilities for barrier models,
  index construction, extraction of fitted spatial fields, posterior
  marginal summaries and model diagnostics.
- **Data.** Coastline and boundary polygons for Ireland and Great
  Britain, including the `barrier` (land) and `area` (prediction domain)
  layers that the barrier models take as input.

The modelling approach is described in Mayer, Mimnagh and Cahill (2026),
*Environmetrics*, <https://doi.org/10.1002/env.70135>. The data and code
for that paper are in
[EireExtremes/stExtremes-paper](https://github.com/EireExtremes/stExtremes-paper).

The GEV and bGEV functions in `R/bgev.R` are not original to this
package. They are taken, essentially verbatim, from the reference
implementation by Daniela Castro-Camilo and Silius M. Vandeskog at
[dcastrocamilo/bGEV](https://github.com/dcastrocamilo/bGEV), and the
file keeps the upstream structure and code style so that it can still be
diffed against the original. The blended GEV itself is due to Vandeskog,
Martino, Castro-Camilo and Rue (2022),
<https://doi.org/10.1007/s13253-022-00500-7>; please cite that paper,
and credit the original authors, when using those functions.

## Installation

You can install the development version of stExtremes with:

``` r
## install.packages("devtools")
devtools::install_github("EireExtremes/stExtremes")
## Or
devtools::install_github("EireExtremes/stExtremes",
    dependencies = TRUE)
```

Note that `INLA` is not on CRAN. Install it from its own repository
first, or use `dependencies = TRUE` above, which picks it up through
`Additional_repositories`:

``` r
install.packages("INLA",
    repos = c(getOption("repos"),
        INLA = "https://inla.r-inla-download.org/R/testing"))
```

## Example

Return levels for the blended GEV. The models in the paper carry
spatio-temporal structure on the location quantile `q`, holding the
spread `s` and the tail `xi` common, so `return_level_bgev2()` takes
that parametrisation directly:

``` r
library(stExtremes)

periods <- c(2, 10, 50, 100)
return_level_bgev2(periods, q = 3.2, sb = 0.45, xi = 0.08)
#> [1] 3.200000 3.777132 4.359834 4.630290
```

With the default `alpha = 0.5`, `q` is the median, so the 2-year level
returns the location itself. The same quantities are available in the
classical parametrisation, and the two agree:

``` r
## (q, s, xi) -> (mu, sigma, xi)
old <- new_to_old(c(3.2, 0.45, 0.08))
unlist(old)
#>        mu     sigma        xi 
#> 3.0974752 0.2756493 0.0800000

return_level_bgev(periods, old$mu, old$sigma, old$xi)
#> [1] 3.200000 3.777132 4.359834 4.630290
```

The bundled layers used by the barrier models:

``` r
data(area)
data(barrier)

## Land, and the sea domain that predictions are made over
class(barrier)
#> [1] "sf"         "data.frame"
sf::st_bbox(area)
#>       xmin       ymin       xmax       ymax 
#> -11.200000  51.000000  -2.384779  55.977999
```

# Acknowledgements

This work has emanated from research conducted with the financial
support of Science Foundation Ireland and co-funded by GSI under Grant
number 20/FFP-P/8610.

<img src="man/figures/logos2.png" alt="" width="100%" />
