# Repository Overview: stExtremes

## Purpose

This repository holds a reusable software toolkit for statistically
modelling extreme sea-level events across space and time. It is built
as an installable software package rather than a one-off analysis, so
that its methods and reference datasets can be shared and reused
consistently across the wider research effort on coastal sea-level
extremes, rather than being re-implemented separately for each
analysis. The work is affiliated with a research group studying sea-
level extremes around Ireland and Great Britain, with acknowledged
support from Science Foundation Ireland.

## What was done

- Established the foundational structure and documentation for a
  statistical software package dedicated to spatio-temporal modelling
  of sea-level extremes.
- Implemented a family of statistical functions for the Generalized
  Extreme Value (GEV) distribution and a related "blended" extension
  of it, including tools to compute densities, probabilities,
  quantiles, random draws, and return levels — the standard building
  blocks used in extreme-value statistics to characterize the
  likelihood and magnitude of rare, high-impact events.
  This code was adapted from an existing published implementation by
  outside researchers and integrated into the package for reuse.
- Built a set of supporting utilities for fitting and interpreting
  spatio-temporal statistical models, including tools to construct
  model input structures, extract fitted spatial patterns, summarize
  and visualize posterior model results, evaluate model fit quality
  and residuals, and produce static and animated visualizations of how
  spatial patterns evolve over time.
- Added utilities for handling geographic/map data, such as cropping
  and defining boundary regions, checking whether spatial points fall
  within a given area, and preparing geographic "barrier" and
  prediction-area definitions needed for spatial modelling near
  coastlines.
- Curated and packaged reference geographic datasets covering Ireland,
  Northern Ireland, Great Britain, and the Isle of Man, produced via
  two alternative approaches (a public geographic-data service and
  official administrative boundary shapefiles), so that consistent,
  ready-to-use maps are available to any analysis built on this
  package.
- Documented the package's functions and datasets and configured it
  for standard installation and distribution.

## Tools and technologies used

- R programming language, developed as a formal, installable R
  package following standard R packaging conventions.
- The `tidyverse` collection of R tools for data manipulation and
  plotting.
- INLA and `inlabru`, specialized R tools for Bayesian spatial and
  spatio-temporal statistical modelling.
- Simple Features (`sf`), a standard format and toolset for
  representing and manipulating geographic vector data (points,
  lines, polygons).
- Extreme value theory / GEV statistical modelling methods, used to
  characterize rare, extreme events.
- Public geographic data sources and official administrative boundary
  shapefiles as inputs for reference map data.
- Animated and static data visualization tooling for illustrating
  spatial patterns over time.
- Version control (git) for tracking development history.

## Role within the broader project

This repository functions as a shared statistical and data toolbox
that underpins other, more applied analysis work within the broader
sea-level extremes research project. Rather than producing a specific
scientific finding itself, it supplies the reusable statistical
methods (extreme value distributions and modelling helpers) and the
standardized geographic reference data (coastlines and boundaries for
Ireland and Great Britain) that downstream analyses depend on to model
and visualize extreme sea-level events consistently. By centralizing
this functionality in one place, it reduces duplicated effort and
ensures that different pieces of the broader project rely on the same
vetted methods and map data.

## Approximate timeline

- **December 2025**: All recorded development took place within a
  single short period early in the month. This included the initial
  creation of the package structure, the core extreme-value
  statistical functions, the spatio-temporal modelling utilities, and
  the curation and packaging of the geographic reference datasets.
  There is no evidence of a later, separate maintenance phase — all
  activity to date represents a single continuous burst of initial
  development.
