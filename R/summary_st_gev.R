##' @title Marginal posterior distributions of the parameters
##'
##' @description This function extracts the marginal posterior
##' distributions of the parameters from an INLA model object.
##'
##' @details The function extracts the marginal posterior distributions
##' of the parameters from an INLA model object. The function returns a
##' list of tibbles, one for each parameter. Each tibble contains the
##' marginal posterior distribution of the parameter.
##'
##' @param mod INLA model object
##'
##' @return A list of tibbles, one for each parameter.
##' @author Fernando Mayer
##'
##' @import INLA tidyverse
##' @importFrom purrr map
##' @export
marginals_st_gev <- function(mod) {
    n_fixed <- length(mod$marginals.fixed)
    names_fixed <- names(mod$marginals.fixed)
    n_hyperpar <- length(mod$marginals.hyperpar)
    names_hyperpar <- names(mod$marginals.hyperpar)
    # Create an empty list for results
    res_marg <- vector("list", length = n_fixed + n_hyperpar)
    names(res_marg) <- c(names_fixed, names_hyperpar)
    # Fixed effects (Identity transformation)
    res_marg[names_fixed] <- lapply(mod$marginals.fixed,
        function(m) inla.tmarginal(identity, m))
    # Define transformation functions for hyperparameters
    hyperpar_transforms <- list(
        "precision for GEV observations" = \(x) 1/sqrt(x),
        "tail parameter for GEV observations" = \(x) x,
        "spread for BGEV observations" = \(x) x,
        "tail for BGEV observations" = \(x) x,
        "Theta1 for w" = \(x) exp(x),
        "Theta2 for w" = \(x) exp(x),
        "GroupRho for w" = \(x) x,
        "Precision for country" = \(x) 1/sqrt(x),
        "Precision for country2" = \(x) 1/sqrt(x),
        "Precision for year_idx" = \(x) 1/sqrt(x),
        "Precision for year_idx2" = \(x) 1/sqrt(x),
        "Rho for year_idx" = \(x) x,
        "Rho for year_idx2" = \(x) x
    )
    # Apply transformations dynamically
    for (nh in names_hyperpar) {
        ## %||% = if(is.null(x)) y else x
        transform_fun <- hyperpar_transforms[[nh]] %||% function(x) x
        res_marg[[nh]] <- inla.tmarginal(transform_fun,
            mod$marginals.hyperpar[[nh]])
    }

    # Define renaming mapping
    rename_map <- c(
        "precision for GEV observations" = "sigmaGEV",
        "tail parameter for GEV observations" = "xiGEV",
        "spread for BGEV observations" = "spread",
        "tail for BGEV observations" = "tail",
        "Theta1 for w" = "rangeM",
        "Theta2 for w" = "sigmaM",
        "GroupRho for w" = "rho",
        "Precision for country" = "prec1",
        "Precision for country2" = "prec2",
        "Precision for year_idx" = "sigma_idx",
        "Precision for year_idx2" = "sigma_idx2",
        "Rho for year_idx" = "rho_idx",
        "Rho for year_idx2" = "rho_idx2"
    )
    # Rename list elements based on mapping
    names(res_marg) <- ifelse(
        names(res_marg) %in% names(rename_map),
        rename_map[names(res_marg)],
        names(res_marg)
    )
    # Convert to tibbles
    res_marg <- purrr::map(res_marg, as_tibble)
    # Assign class
    class(res_marg) <- c("res_marg", class(res_marg))
    return(res_marg)
}

##' @title Summary of the marginal posterior distributions of the
##' parameters
##'
##' @description This function summarizes the marginal posterior
##' distributions of the parameters.
##'
##' @details The function summarizes the marginal posterior
##' distributions of the parameters. The function returns a table with
##' the mean, median, mode, standard deviation, 2.5% quantile, and 97.5%
##' quantile of the marginal posterior distributions of the parameters.
##'
##' @param object A list of tibbles, one for each parameter.
##' @param round Number of decimal places to round the results.
##' @param ... Additional arguments to be passed to the summary function.
##'
##' @return A table with the mean, median, mode, standard deviation,
##' 2.5% quantile, and 97.5% quantile of the marginal posterior
##' distributions of the parameters.
##' @author Fernando Mayer
##'
##' @import INLA tidyverse
##' @importFrom purrr map
##' @method summary res_marg
##' @export
summary.res_marg <- function(object, round = NULL, ...) {
    marginals <- object
    ## Summary table
    res_tab <- purrr::map(marginals,
        function(x) inla.zmarginal(x, silent = TRUE))
    res_tab <- purrr::map(res_tab, as_tibble) |>
        bind_rows(.id = "parameter")
    res_tab <- res_tab[, -c(5, 7)]
    if(!is.null(round)) {
        res_tab <- res_tab |>
            mutate(across(where(is.numeric), \(x) round(x, round)))
    }
    ## UseMethod("summary")
    return(res_tab)
}

##' @title Plot marginal posterior distributions of the parameters
##'
##' @description This function plots the marginal posterior
##' distributions of the parameters.
##'
##' @details The function plots the marginal posterior distributions of
##' the parameters. The function returns a plot with the marginal
##' posterior distributions of the parameters.
##'
##' @param x A list of tibbles, one for each parameter.
##' @param ... Additional arguments to be passed to the plot function.
##'
##' @return A plot with the marginal posterior distributions of the
##' parameters.
##'
##' @author Fernando Mayer
##'
##' @import ggplot2 dplyr
##' @importFrom forcats fct_relevel
##' @importFrom purrr map
##' @method plot res_marg
##' @export
plot.res_marg <- function(x, ...) {
    marginals <- x
    res_marg <- purrr::map(marginals, as_tibble) |>
        bind_rows(.id = "Parameter") |>
        mutate(
            Parameter =
                forcats::fct_relevel(Parameter, names(marginals))
        )
    ggplot(res_marg, aes(x = x, y = y)) +
        geom_line() +
        facet_wrap(~ Parameter, scales = "free",
            nrow = nrow(res_marg)) +
        xlab("") + ylab("Density")
    ## UseMethod("plot")
}
