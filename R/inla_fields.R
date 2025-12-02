##' @title Field functions
##'
##' @description Functions to extract and plot fields from INLA models
##' with spatial and temporal components.
##'
##' @details The functions \code{mfield} and \code{sfield} extract the
##' mean and standard deviation fields from an INLA model with spatial
##' and temporal components. The function \code{plot_field} plots the
##' field at a given time.
##'
##' @param m INLA model
##' @param index Index of the time variable in the data
##' @param projgrid Projected grid to project the field to the original
##' grid of the data (not the mesh)
##' @param crs The Coordinate Reference System of the data points
##'
##' @return A list with the mean field at each time point
##' @author Fernando Mayer
##'
##' @importFrom INLA inla.mesh.project
##'
##' @name fields
##' @export
mfield <- function(m, index, projgrid, crs) {
    ntime <- unique(index[[2]])
    meanf <- vector("list", length(ntime))
    for(i in seq_along(ntime)) {
        meanf[[i]] <- INLA::inla.mesh.project(
            projgrid,
            m$summary.random[[1]]$mean[index[[2]] == i],
            crs = crs
        )
    }
    return(meanf)
}

##' @rdname fields
##' @export
sfield <- function(m, index, projgrid, crs) {
    ntime <- unique(index[[2]])
    sdf <- vector("list", length(ntime))
    for(i in seq_along(ntime)) {
        sdf[[i]] <- INLA::inla.mesh.project(
            projgrid,
            m$summary.random[[1]]$sd[index[[2]] == i],
            crs = crs
        )
    }
    return(sdf)
}
