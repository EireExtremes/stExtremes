##' @title General summary of a model diagnostics
##'
##' @description This function returns a data frame with the main diagnostics of
##'     an INLA model (DIC, WAIC, lCPO, etc).
##'
##' @details NEEDS UPDATE. The function returns a data frame with the following
##'     columns: DIC, NP.DIC, WAIC, NP.WAIC, CPO, FAIL, NFAIL, CPU. The first
##'     four columns are the main diagnostics of the model, the next two are the
##'     cross-validation diagnostics, and the last one is the CPU time used by
##'     the model.
##'
##' @param mod An object of class \code{inla}
##' @param y The (observed) response variable
##' @param idx The index from INLA stack
##' @param fun Function used to summarise the values
##' @param na.rm Remove NAs (defaults to \code{TRUE})
##' @param KIS Keep It Simple: keep extrictly the main measures only (don't show
##'     number of parameters, fails and CPU time)
##'
##' @return A named vector with the main diagnostics of the model.
##' @author Fernando Mayer
##'
##' @export
mod_summary <- function(mod, y, idx, fun = sum, na.rm = TRUE, KIS = TRUE) {
    res <- c(
        DIC = mod$dic$dic,
        NP.DIC = mod$dic$p.eff,
        WAIC = mod$waic$waic,
        NP.WAIC = mod$waic$p.eff,
        lCPO = -fun(log(mod$cpo$cpo), na.rm = na.rm),
        FAIL = fun(mod$cpo$failure, na.rm = na.rm),
        NFAIL = fun(mod$cpo$failure > 0, na.rm = na.rm),
        lPO = -fun(log(mod$po$po), na.rm = na.rm),
        MSE = fun((y[idx] - mod$summary.fitted.value$mean[idx])^2,
            na.rm = na.rm),
        MAE = fun(abs(y[idx] - mod$summary.fitted.value$mean[idx]),
            na.rm = na.rm),
        CPU = unname(mod$cpu.used["Total"])
    )
    if(KIS) {
       res <- res[names(res) %in%
                  c("DIC", "WAIC", "lCPO", "lPO", "MSE", "MAE")]
    }
    return(res)
}
