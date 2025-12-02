##' @title Get spatial effect from an INLA model
##'
##' @description Get spatial effect from an INLA model and return it as
##' a data frame.
##'
##' @details This function extracts the spatial effect from an INLA
##' model and returns it as a data frame. The data frame has three
##' columns corresponding to the three quantiles of the spatial effect.
##'
##' @param mod An INLA model object.
##' @param stk_idx Stacked indexes of the observations.
##'
##' @return A data frame with three columns corresponding to the three
##' quantiles of the spatial effect.
##' @author Fernando Mayer
##' @export
get_spatial_effect <- function(mod, stk_idx) {
    ## Get stacked indexes
    idat <- stk_idx
    rnd <- mod$summary.random$w[idat, 4:6]
    names(rnd) <- c("Q1.eff", "Q2.eff", "Q3.eff")
    class(rnd) <- c("sp_effect", class(rnd))
    return(rnd)
}

## FIX this
## plot.sp_effect <- function(sfield, ...) {
##     stopifnot(inherits(sfield, "sp_effect"))
##     ggplot(sfield, aes(x = 1:nrow(sfield))) +
##         geom_line(aes(y = Q1), lty = 1) +
##         geom_line(aes(y = Q2), lty = 2) +
##         geom_line(aes(y = Q3), lty = 1) +
##         xlab("Observation index") +
##         ylab("Spatial effect (residual)")
## }
