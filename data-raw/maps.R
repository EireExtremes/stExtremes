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
shp_roi <- ne_countries(country = "ireland",
    scale = "large",
    returnclass = "sf") |>
    st_transform(crs = proj)

shp_gbr <- ne_countries(country = "united kingdom",
    scale = "large",
    returnclass = "sf") |>
    st_transform(crs = proj)

shp_imn <- ne_countries(country = "isle of man",
    scale = "large",
    returnclass = "sf") |>
    st_transform(crs = proj)

## Join GBR and IMN
shp_gbr <- st_union(st_geometry(shp_gbr), st_geometry(shp_imn)) |>
    st_as_sf()

## Join ROI and GBR
shp_all <- st_union(st_geometry(shp_roi), st_geometry(shp_gbr)) |>
    st_as_sf() |>
    st_make_valid()

## Plot to verify
ggplot() +
    geom_sf(data = shp_all)


##----------------------------------------------------------------------
## Save
(fl <- "data/map_roi.rda")
if(!file.exists(fl)) {
    save(shp_roi, file = fl, compress = "xz")
}

(fl <- "data/map_gbr.rda")
if(!file.exists(fl)) {
    save(shp_gbr, file = fl, compress = "xz")
}

(fl <- "data/map_all.rda")
if(!file.exists(fl)) {
    save(shp_all, file = fl, compress = "xz")
}
