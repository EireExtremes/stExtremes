##==============================================================================
## Create the base maps
##==============================================================================

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

## Define a common projection
proj <- st_crs("+proj=longlat +datum=WGS84")

## Get country polygons at high resolution
map_roi <- ne_countries(country = "ireland",
    scale = "large",
    returnclass = "sf") |>
    st_transform(crs = proj)

map_gbr <- ne_countries(country = "united kingdom",
    scale = "large",
    returnclass = "sf") |>
    st_transform(crs = proj)

map_imn <- ne_countries(country = "isle of man",
    scale = "large",
    returnclass = "sf") |>
    st_transform(crs = proj)

## Join GBR and IMN
map_gbr <- st_union(st_geometry(map_gbr), st_geometry(map_imn)) |>
    st_as_sf()

## Join ROI and GBR
map_all <- st_union(st_geometry(map_roi), st_geometry(map_gbr)) |>
    st_as_sf() |>
    st_make_valid()

## Plot to verify
ggplot() +
    geom_sf(data = map_all)


##----------------------------------------------------------------------
## Save
(fl <- "data/map_roi.rda")
if(!file.exists(fl)) {
    save(map_roi, file = fl, compress = "xz")
}

(fl <- "data/map_gbr.rda")
if(!file.exists(fl)) {
    save(map_gbr, file = fl, compress = "xz")
}

(fl <- "data/map_all.rda")
if(!file.exists(fl)) {
    save(map_all, file = fl, compress = "xz")
}
