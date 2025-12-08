##======================================================================
## Create maps from shapefiles

##----------------------------------------------------------------------
## Packages
pkgs <- c("tidyverse", "sf")
sapply(pkgs, library, character.only = TRUE, quietly = TRUE,
    verbose = FALSE, logical.return = TRUE, warn.conflicts = FALSE)
theme_set(theme_bw())

##----------------------------------------------------------------------
## Import all the shapefiles

## Define a common projection ------------------------------------------
proj <- st_crs("+proj=longlat +datum=WGS84")

## Download shapefiles -------------------------------------------------

## TODO: place these files in the AWS S3 bucket
## All downloaded from https://gadm.org

## Shapefile for ROI ---------------------------------------------------
fl <- "data-raw/gadm41_IRL_shp.zip"
if(!file.exists(fl)) {
    options(timeout = 600)
    download.file(
        url = "http://leg.ufpr.br/~fernandomayer/misc/gadm41_IRL_shp.zip",
        destfile = fl
    )
    ## Unzip the file
    unzip(
        zipfile = fl,
        exdir = "data-raw/gadm41_IRL_shp"
    )
}

## Read the shapefile
dsn_roi <- "data-raw/gadm41_IRL_shp/"
## Select the appropriate layer
shp_roi <- st_read(dsn = dsn_roi, layer = "gadm41_IRL_0")
## Convert to common projection
shp_roi <- st_transform(shp_roi, crs = proj)
## Plot
ggplot() +
    geom_sf(data = shp_roi)

## Shapefile for GBR ---------------------------------------------------
fl <- "data-raw/gadm41_GBR_shp.zip"
if(!file.exists(fl)) {
    options(timeout = 600)
    download.file(
        url = "http://leg.ufpr.br/~fernandomayer/misc/gadm41_GBR_shp.zip",
        destfile = fl
    )
    ## Unzip the file
    unzip(
        zipfile = fl,
        exdir = "data-raw/gadm41_GBR_shp"
    )
}

## Read the shapefile
dsn_gbr <- "data-raw/gadm41_GBR_shp/"
## Select the appropriate layer
shp_gbr <- st_read(dsn = dsn_gbr, layer = "gadm41_GBR_1")
## Convert to common projection
shp_gbr <- st_transform(shp_gbr, crs = proj)
## Plot
ggplot() +
    geom_sf(data = shp_gbr)

## Shapefiles for the Isle of Man --------------------------------------
fl <- "data-raw/gadm41_IMN_shp.zip"
if(!file.exists(fl)) {
    options(timeout = 600)
    download.file(
        url = "http://leg.ufpr.br/~fernandomayer/misc/gadm41_IMN_shp.zip",
        destfile = "data-raw/gadm41_IMN_shp.zip"
    )
    ## Unzip the file
    unzip(
        zipfile = "data-raw/gadm41_IMN_shp.zip",
        exdir = "data-raw/gadm41_IMN_shp"
    )
}

## Read the shapefile
dsn_imn <- "data-raw/gadm41_IMN_shp/"
## Select the appropriate layer
shp_imn <- st_read(dsn = dsn_imn, layer = "gadm41_IMN_0")
## Convert to common projection
shp_imn <- st_transform(shp_imn, crs = proj)
## Plot
ggplot() +
    geom_sf(data = shp_imn)

##----------------------------------------------------------------------
## Check classes and geometries
class(shp_roi)
class(shp_gbr)
class(shp_imn)

st_geometry_type(shp_roi)
st_geometry_type(shp_gbr)
st_geometry_type(shp_imn)

## Make the uniion of GBR "pieces"
shp_gbr <- st_union(shp_gbr)
class(shp_gbr)
st_geometry_type(shp_gbr)
shp_gbr <- st_as_sf(shp_gbr)
class(shp_gbr)

##----------------------------------------------------------------------
## Join GBR and IMN
shp_gbr <- st_union(st_geometry(shp_gbr), st_geometry(shp_imn))
class(shp_gbr)
st_geometry_type(shp_gbr)
shp_gbr <- st_as_sf(shp_gbr)
ggplot() +
    geom_sf(data = shp_gbr)

## Save
(fl <- "data/shp_gbr.rda")
if(!file.exists(fl)) {
    save(shp_gbr, file = fl, compress = "xz")
}

##----------------------------------------------------------------------
## Join ROI and GBR
shp_all <- st_union(st_geometry(shp_roi), st_geometry(shp_gbr))
class(shp_all)
st_geometry_type(shp_all)
shp_all <- st_as_sf(shp_all)
st_is_valid(shp_all)
shp_all <- st_make_valid(shp_all)
st_is_valid(shp_all)
ggplot() +
    geom_sf(data = shp_all)

## Save
(fl <- "data/shp_all.rda")
if(!file.exists(fl)) {
    save(shp_all, file = fl, compress = "xz")
}

##==============================================================================
## For INLA: define the berrier and the area

## Define lat/lon range for the final map
llr <- c(-11.2, 51, -2.3, 55.9)

## Cut coastline - define the BARRIERS
barrier <- crop_layer(shp_all,
    xmin = llr[1], ymin = llr[2], xmax = llr[3], ymax = llr[4])

## Checks
class(barrier)
st_geometry_type(barrier)
st_is_valid(barrier)
st_crs(barrier) == proj
ggplot() +
    geom_sf(data = barrier)

## Define AREA (polygon) for prediction - all minus the barrier
area <- poly_pred(barrier, xmin = llr[1], ymin = llr[2],
    xmax = llr[3], ymax = llr[4])

## Checks
class(area)
st_geometry_type(area)
st_is_valid(area)
area <- st_make_valid(area)
st_crs(area) == proj

ggplot() +
    geom_sf(data = area, fill = "lightblue") +
    geom_sf(data = barrier, fill = "lightgrey")

## To create the mesh, area must be a POLYGON or MULTIPOLYGON.
## Here it is a MULTILINESTRING as well as a MULTIPOLYGON. The
## MULTILINESTRING part is just an unconnected border around the area,
## so we can extract the POLYGON part only.
area_poly <- st_collection_extract(area, "POLYGON")
area_lines <- st_collection_extract(area, "LINESTRING")

## Checks
st_geometry_type(area_poly)
st_geometry_type(area_lines)
ggplot() +
    geom_sf(data = area_poly, fill = "lightblue")

## Make the default
area <- area_poly

## Checks
st_crs(area) == proj
st_crs(barrier) == proj
ggplot() +
    geom_sf(data = area, fill = "lightblue") +
    geom_sf(data = barrier, fill = "lightgrey")


## Save
(fl <- "data/area.rda")
if(!file.exists(fl)) {
    save(area, file = fl, compress = "xz")
}

(fl <- "data/barrier.rda")
if(!file.exists(fl)) {
    save(barrier, file = fl, compress = "xz")
}
