##' @title Identify triangles on a mesh that are within a polygon
##'
##' @description This function identifies the triangles on a mesh that
##' are within a polygon.
##'
##' @details This function uses the \code{fmesher} package to identify
##' the triangles on a mesh that are within a polygon. The function
##' returns a vector of integers indicating the triangles that are
##' within the polygon and a \code{tibble} with the centroids of the
##' triangles that are within the polygon.
##'
##' @param mesh An \code{inla.mesh} object
##' @param polygon A \code{sf} object
##'
##' @return A vector of integers indicating the triangles that are
##' within the polygon and a \code{tibble} with the centroids of the
##' triangles that are within the polygon.
##' @author Fernando Mayer
##'
##' @import fmesher INLA sf tibble dplyr tidyr
##' @export
tri_on_mesh <- function(mesh, polygon, inverse = FALSE) {
    tri_barrier <- unlist(
        fmesher::fm_contains(
                     x = polygon,
                     y = mesh,
                     type = "centroid")
    )
    tri_barrier_centres <- cbind(
        mesh$loc[mesh$graph$tv[, 1], 1:2] +
        mesh$loc[mesh$graph$tv[, 2], 1:2] +
        mesh$loc[mesh$graph$tv[, 3], 1:2])/3
    if(inverse) {
        tri_barrier <- setdiff(1:nrow(tri_barrier_centres), tri_barrier)
    }
    attr(tri_barrier, "centres") <- tri_barrier_centres
    attr(tri_barrier, "centres") <- attr(tri_barrier, "centres") |>
        as_tibble(.name_repair = ~ c("x", "y"))
    class(tri_barrier) <- c("tri_on_mesh", class(tri_barrier))
    ## UseMethod("print")
    return(tri_barrier)
}

##' @title Plot triangles on a mesh
##'
##' @description This function plots the triangles on a mesh that are
##' within a polygon.
##'
##' @param x A \code{tri_on_mesh} object
##' @param mesh An \code{inla.mesh} object
##' @param all A logical indicating if all triangles should be plotted
##' or only the triangles that are within the polygon
##' @param ... Additional arguments to be passed to
##' \code{\link[ggplot2]{geom_point}} function
##'
##' @return A ggplot object
##' @author Fernando Mayer
##'
##' @import ggplot2
##' @importFrom inlabru gg
##'
##' @method plot tri_on_mesh
##' @export
plot.tri_on_mesh <- function(x, mesh, all = FALSE, ...) {
    tri_obj <- x
    if(all) {
        dt <- attr(tri_obj, "centres")
    } else {
        dt <- attr(tri_obj, "centres")[as.integer(tri_obj), ]
    }
    ggplot() +
        gg(data = mesh) +
        geom_point(aes(x = x, y = y), data = dt)
    ## UseMethod("plot")
}
