##' @title Check for points on a polygon
##'
##' @description This function checks if there are points on a polygon.
##'
##' @details This function checks if there are points on a polygon. If
##' there are points on the polygon, it returns the number of points and
##' the coordinates of the points. If there are no points on the
##' polygon, it returns only the number of points.
##'
##' @param points A sf object with points (usually of class
##' \code{sfc_POINT})
##' @param polygon A sf object with a polygon
##'
##' @return A list with the number of points on the polygon and the
##' coordinates of the points on the polygon. If there are no points on
##' the polygon, only the number of points is returned.
##' @author Fernando Mayer
##'
##' @import sf
##' @export
check_points <- function(points, polygon) {
    stopifnot(inherits(points$geometry, "sfc_POINT"))
    ## stopifnot(inherits(polygon, "sfc_GEOMETRY"))
    ## Check for points on polygon
    points_on_poly <- st_filter(points, polygon)
    ## Count the number of points on land
    n_points_on_poly <- nrow(points_on_poly)
    if (n_points_on_poly > 0) {
        ## If there are points on land, extract their coordinates
        points_on_poly_coords <- points_on_poly |>
            distinct(geometry) |>
            as_tibble()
        ## Return the count and coordinates of points on land
        return(list(
            count = n_points_on_poly,
            coordinates = points_on_poly_coords,
            n_coordinates = nrow(points_on_poly_coords))
        )
    } else {
        ## If there are no points on land, return only the count
        return(list(
            count = n_points_on_poly)
        )
    }
}
