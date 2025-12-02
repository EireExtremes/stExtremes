##' @title Create Indices for the Spatial Field and Temporal Components
##'
##' @description This function creates indices for the spatial field and
##' temporal components.
##'
##' @details The function creates indices for the spatial field and
##' temporal components. The function is used to create the indices for
##' the INLA model.
##'
##' @param time_indices A vector with the time indices.
##' @param mesh A mesh object.
##'
##' @return A list with the indices for the spatial field and temporal
##' components.
##' @author Fernando Mayer
##'
##' @importFrom INLA inla.spde.make.index
##' @importFrom utils head
##' @export
create_indices <- function(time_indices, mesh) {
    ## Define indexes by year_idx (ydx)
    ydx <- time_indices
    n_ydx <- length(unique(ydx))
    n_knot <- mesh$n
    ## Indices for the spatial field
    w.index <- inla.spde.make.index("w", n.spde = n_knot, n.group = n_ydx)
    ## Extracting information
    spatial_vertex_indices <- table(w.index$w)
    temporal_indices <- table(w.index$w.group)
    repetition_indices <- table(w.index$w.repl)
    ## Printing information
    cat("Spatial Vertex Indices:\n")
    print(utils::head(spatial_vertex_indices))
    cat("\nTemporal Indices:\n")
    print(temporal_indices)
    cat("\nRepetition Indices:\n")
    print(repetition_indices)
    ## Return
    return(w.index)
}
