##' @title Get HPD intervals for fixed effects from INLA model
##'
##' @description Get HPD intervals for fixed effects from INLA model
##' using the inla.hpdmarginal function from the INLA package.
##'
##' @details The function takes an INLA model object and a probability
##' level p (default is 0.95) and returns a data frame with the HPD
##' intervals for each fixed effect. The intervals are calculated using
##' the inla.hpdmarginal function from the INLA package.
##'
##' @param mod INLA model object
##' @param p Probability level for the HPD interval
##'
##' @return A data frame with the HPD intervals for each fixed effect in
##' the model object.
##' @author Fernando Mayer
##'
##' @importFrom INLA inla.hpdmarginal
##' @importFrom data.table rbindlist
##' @export
get_hpd <- function(mod, p = 0.95) {
    marg <- mod$marginals.fixed
    res <- lapply(marg, function(x)
        INLA::inla.hpdmarginal(p = p, marginal = x))
    res <- lapply(res, as.data.frame)
    res <- as.data.frame(data.table::rbindlist(res, idcol = TRUE))
    names(res)[1] <- "covar"
    res$inout <- ifelse(
        apply(res[, 2:3], 1, function(x) {
            all(x > 0) | all(x < 0)
        }),
        "nin0", "in0")
    return(res)
}
