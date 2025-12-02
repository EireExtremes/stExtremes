##' @title Crop a layer
##'
##' @description Crop a layer to a specified extent using an \code{sf}
##' object as the extent.
##'
##' @details This function crops a layer to a specified extent using an
##' \code{sf} object as the extent. The function takes an \code{sf}
##' object and the minimum and maximum x and y coordinates of the extent
##' to crop the layer. The function returns an \code{sf} object with the
##' same projection as the input layer.
##'
##' @param shape An \code{sf} object
##' @param xmin The minimum x-coordinate
##' @param ymin The minimum y-coordinate
##' @param xmax The maximum x-coordinate
##' @param ymax The maximum y-coordinate
##' @param crs The coordinate reference system of the output layer. If
##' \code{NULL}, the CRS of the input layer is used.
##'
##' @return An \code{sf} object
##' @author Fernando Mayer
##'
##' @import sf
##' @export
crop_layer <- function(shape, xmin, ymin, xmax, ymax, crs = NULL) {
    ## stopifnot(inherits(shape, "sf"))
    ## Get the projection of the input layer
    if(is.null(crs)) {
        proj <- st_crs(shape)
    } else {
        proj <- crs
    }
    ## Get the bounding box of the input layer
    bbox <- st_bbox(shape)
    ## Define the extent
    extent <- st_bbox(c(xmin = xmin, ymin = ymin,
        xmax = xmax, ymax = ymax))
    ## Convert extent to a polygon
    spatial_polygons <- st_as_sfc(extent, crs = proj)
    ## Assign to layer_crop
    layer_crop <- st_sf(geometry = spatial_polygons)
    ## Assign the CRS
    st_crs(layer_crop) <- proj
    ## Crop the input layer
    layer_crop <- st_crop(shape, layer_crop)
    return(layer_crop)
}
