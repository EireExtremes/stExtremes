##' @title Get stacked indexes from INLA object
##'
##' @description Get stacked indexes from INLA object for a given type
##' (est, obs, pred) and return them as a data frame.
##'
##' @details This function is a wrapper for the \code{inla.stack.index}
##' function from the \code{INLA} package. It returns the indexes of the
##' stacked data for a given type (est, obs, pred) as a data frame.
##'
##' @param stk INLA object to extract indexes from (output from
##' \code{inla}).
##' @param type Type of indexes to extract (est, obs, pred).
##'
##' @return A data frame with the indexes of the stacked data.
##' @author Fernando Mayer
##'
##' @importFrom INLA inla.stack.index
##' @export
get_stack_index <- function(stk, type = "est") {
    ## Get stacked indexes
    idat <- INLA::inla.stack.index(stk, type)$data
    return(idat)
}
