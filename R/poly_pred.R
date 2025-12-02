##' @title Create a polygon for the spatial prediction
##'
##' @description This function creates a polygon with the area to
##' predict based on the input shapefile and the coordinates of the
##' area.
##'
##' @details The function receives a shapefile and the coordinates of
##' the area to predict. The function creates a polygon with the area to
##' predict based on the input shapefile and the coordinates of the area.
##'
##' @inheritParams crop_layer
##' @return A polygon with the area to predict
##' @author Fernando Mayer
##'
##' @import sf
##' @export
poly_pred <- function(shape, xmin, ymin, xmax, ymax, crs = NULL) {
    stopifnot(inherits(shape, "sf"))
    ## Get the projection of the input layer
    if(is.null(crs)) {
        proj <- st_crs(shape)
    } else {
        proj <- crs
    }
    ## Define the coordinates
    coords <- matrix(
        c(xmin, ymin,
            xmin, ymax,
            xmax, ymax,
            xmax, ymin,
            xmin, ymin),
        ncol = 2, byrow = TRUE
    )
    ## Create the polygon with the coordinates
    poly_area <- st_sfc(
        st_multipolygon(list(st_polygon(list(coords)))),
        crs = proj
    )
    ## Clip the polygon with the final shape
    poly_final <- st_difference(poly_area, shape)
    return(poly_final)
}
