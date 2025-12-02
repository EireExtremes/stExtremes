##' @title Get fit and residuals from INLA model
##'
##' @description Get fit and residuals from INLA model for a given
##' dataset and model.
##'
##' @details This function extracts the fitted values and residuals from
##' an INLA model for a given dataset. The function can also be used to
##' extract the fitted values and residuals from a given function.
##'
##' @param mod An INLA model object.
##' @param y A numeric vector of observed values.
##' @param idat A numeric vector of indices.
##' @param fun A function to extract the fitted values and residuals.
##' @param std A logical value indicating whether to standardize the
##' residuals. Default is \code{TRUE}.
##'
##' @return A data frame with the fitted values and residuals.
##' @author Fernando Mayer
##'
##' @importFrom INLA inla.tmarginal inla.zmarginal
##' @importFrom parallel mclapply detectCores
##' @export
get_fit_res <- function(mod, y, idat, fun = NULL, std = TRUE){
    ncores <- as.integer(parallel::detectCores() - 2)
    if(is.null(fun)){
        fit.mean <- mod$summary.fitted.values$mean[idat]
        fit.sd <- mod$summary.fitted.values$sd[idat]
        fit.q <- mod$summary.fitted.values[idat, 3:5]
        names(fit.q) <- c("Q1.fit", "Q2.fit", "Q3.fit")
    } else{
        marg <- mod$marginals.fitted.values
        tmarg <- parallel::mclapply(marg, function(x){
            INLA::inla.tmarginal(fun = fun, x)
        }, mc.cores = ncores)
        res.marg <- parallel::mclapply(tmarg, function(x){
            unlist(INLA::inla.zmarginal(x, silent = TRUE)[1:2])
        }, mc.cores = ncores)
        ## res.marg <- t(res.marg)
        res.marg <- do.call(rbind, res.marg)
        res.marg <- as.data.frame(res.marg,
            row.names = 1:nrow(res.marg))
        fit.mean <- res.marg$mean
        fit.sd <- res.marg$sd
    }
    if(std){
        res <- (y - fit.mean)/fit.sd
    } else{
        res <- (y - fit.mean)
    }
    out <- cbind(fit.q, res = res)
    class(out) <- c("fit_res", class(out))
    return(out)
}

## TODO
## plot.fit_res <- function(data, obj, facet = NULL, ...) {
##     stopifnot(inherits(obj, "fit_res"))
##     ggplot(db_max_sp3, aes(x = max)) +
##     geom_point(aes(y = Q2)) +
##     facet_wrap(~ site_name, scales = "free_y", nrow = 3)
## }
