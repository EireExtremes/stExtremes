# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Package overview

`stExtremes` is an R package (maintained by Fernando Mayer and Niamh Cahill,
Maynooth University) providing tools for spatio-temporal modelling of
sea-level extremes with INLA/inlabru. It combines:

- GEV / bGEV (blended GEV) distribution functions and negative
  log-likelihoods for extreme value analysis.
- Helper functions that wrap the INLA/inlabru SPDE workflow (mesh
  triangles, stack indices, spatial fields, posterior marginals, HPD
  intervals, residuals, model diagnostics).
- Bundled `sf` geographic datasets (Ireland/Great Britain coastlines and
  barrier/prediction polygons) used as inputs to that INLA workflow.

The package depends on `INLA` and `inlabru`, which are not on CRAN; INLA is
installed from `https://inla.r-inla-download.org/R/testing/` (see
`Additional_repositories` in `DESCRIPTION`). It also imports `geslaR`, a
sibling package (Global Extreme Sea Level Analysis).

### Role within the wider project

This is deliberately a reusable, installable toolkit rather than a
self-contained analysis: it produces no scientific findings of its own. It
exists so that the extreme-value methods and the standardised geographic
reference data are defined once and shared, instead of being re-implemented
per analysis. The applied work that uses it — data pipelines, model fitting,
and the paper — lives in a separate private development repository, which is
also where these functions originate. Changes therefore tend to flow
dev -> public, so when editing `R/`, check whether the change belongs
upstream too, and keep the two copies in sync.

The work is supported by Science Foundation Ireland, co-funded by GSI, under
Grant number 20/FFP-P/8610 (see `README.Rmd`).

## Common commands

Development uses the standard `devtools`/`roxygen2` workflow (devtools is
auto-loaded in interactive sessions via `.Rprofile`).

```r
devtools::load_all()      # load package for interactive development
devtools::document()      # regenerate NAMESPACE and man/*.Rd from roxygen
devtools::check()         # full R CMD check
devtools::install()       # install locally
```

From the shell:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::check()'
R CMD build .
R CMD check stExtremes_*.tar.gz
```

There are no unit tests in this package currently (no `tests/` directory).

Roxygen is configured with `Roxygen: list(markdown = TRUE)` in `DESCRIPTION`,
so `.Rd` files in `man/` are generated — always edit the roxygen comments in
`R/*.R` and re-run `devtools::document()` rather than editing `man/*.Rd`
directly.

## Architecture

### `R/bgev.R` — verbatim ported code, do not restyle

This file (the largest in the package) is an explicit line-for-line copy of
code from https://github.com/dcastrocamilo/bGEV (by Daniela Castro-Camilo and
Silius M.V.), organized into clearly marked sections ("Copy of
bGEV/Code/utils.R", "...bGEVcode.R", "...GEVcode.R",
"...GEVbGEVlikelihoods.R", "...utilsBakersfield.R"). It intentionally uses
`=` for assignment and different spacing than the rest of the package. Leave
its style as-is when editing nearby — don't "fix" it to match tidyverse
conventions, since divergence from the upstream source would make future
diffs against `bGEV` harder to follow. All of these functions are exported
but documented as `@keywords internal` (see `?gev_internals`): they exist for
use by other functions/packages, not as the primary user-facing API.

Key building blocks in this file: `pgev`/`qgev`/`dgev`/`rgev` (standard GEV),
`pbgev`/`qbgev`/`dbgev`/`rbgev` (blended GEV), `*2` variants using an
alternate (quantile-based) parametrisation via `new_to_old`/`old_to_new`,
`nllik_*` negative log-likelihood functions for optimization, and
`get_gumbel_par`/`map_tail`/`fix_lengths` utilities.

### INLA/inlabru workflow helpers (rest of `R/`)

The remaining files are original code (author: Fernando Mayer) supporting a
typical spatio-temporal SPDE-INLA analysis pipeline, roughly in the order
they'd be used:

- `create_indices.R`, `get_stack_index.R` — build SPDE spatial/temporal
  indices and pull indices back out of an `inla.stack`.
- `tri_on_mesh.R`, `check_points.R`, `crop_layer.R`, `poly_pred.R` — mesh
  and polygon geometry utilities (which mesh triangles fall in a barrier
  polygon, cropping/complementing `sf` layers to build barrier/prediction
  areas).
- `inla_fields.R` (`mfield`/`sfield`), `spatial_effect.R`
  (`get_spatial_effect`) — extract fitted spatial random fields from a
  fitted `inla`/`inlabru` model.
- `summary_st_gev.R` (`marginals_st_gev`, plus `summary.res_marg` and
  `plot.res_marg` S3 methods) — extract and transform posterior marginals
  of fixed/hyperparameters into a `res_marg` object; hyperparameter
  transforms and renaming are hardcoded to this package's specific model
  parametrisation (e.g. `"Theta1 for w"` -> `rangeM`), so extend the
  `hyperpar_transforms`/`rename_map` lists there when adding new
  model components.
- `get_hpd.R`, `mod_summary.R`, `inla_residuals.R` (`get_fit_res`) —
  posterior HPD intervals, DIC/WAIC/CPO diagnostics, and
  fitted/residual extraction.
- `field_plot.R`, `anim_plot.R` — `ggplot2`/`gganimate` visualisation of
  spatial fields over time (static facet plot vs. animated GIF); both
  reshape a list of field matrices (one per year) into long `sf` format
  before plotting.

### Bundled data (`data/`, built by `data-raw/`)

Two independent sources produce overlapping datasets — know which is
current before adding to either:

- `data-raw/maps.R` builds `map_roi`, `map_irl`, `map_gbr`, `map_all` from
  `rnaturalearth`/`rnaturalearthdata` at `scale = "large"`. This is the
  preferred route going forward, as it needs no downloaded files.
- `data-raw/maps-shp.R` builds `shp_all`, `shp_gbr` (and the modelling
  inputs `area`, `barrier`) from GADM shapefiles downloaded into
  `data-raw/gadm41_*_shp/`. `barrier` is the coastline/land polygon cropped
  to a fixed lat/lon box; `area` is its complement within that box (the
  prediction domain for the INLA mesh) — both are consumed directly by the
  SPDE mesh-building step of the modelling workflow, not just for plotting.

Both routes compose the same way, so the names understate what they hold:
the Isle of Man is unioned into `map_gbr`/`shp_gbr`, `map_irl` is the
Republic plus Northern Ireland, and `map_all`/`shp_all` is the Republic
unioned with Great Britain (Isle of Man included).

All `data-raw/*.R` scripts guard `save()` calls with
`if (!file.exists(fl))`, so re-running them will not overwrite existing
`.rda` files in `data/` — delete the target file first if you need to
regenerate one. Document new datasets via `@format`/`"name"` roxygen blocks
in `R/data.R`.

`DESCRIPTION` sets `LazyData: true`, so datasets are available on load and
need no `@export` tag; the object saved inside each `.rda` must match its
file name, or `document()` fails claiming the object is not exported.

## Style conventions

Most `R/*.R` files (all except `R/bgev.R`) use roxygen comment blocks
prefixed with `##'` (double hash) rather than the standard single `#'` —
follow this local convention when adding documentation to those files.
`R/data.R` is the one exception using standard `#'`.

Otherwise, follow the global R conventions (tidyverse pipe `|>`, `<-` for
assignment, `##`/`#` comment style) — these apply throughout except in the
verbatim-ported `R/bgev.R` described above.
