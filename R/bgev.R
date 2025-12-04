##================================================================================
## All functions here were extracted from
## https://github.com/dcastrocamilo/bGEV/tree/master
## and were developed by Daniela Castro-Camilo
## NOTE see how to cite this properly in de docs
##==============================================================================

##' Internal GEV helpers
##'
##' Random generation, density, distribution and related functions for the GEV
##' distribution. These are exported primarily for use by other packages and
##' advanced users, and are not part of the stable user-facing API.
##'
##' @keywords internal
##' @name gev_internals
NULL

##==============================================================================
## Copy of bGEV/Code/utils.R

#################################################
## Utility functions to work with the bGEV-GEV ##
#################################################
# By Silius M.V. and Daniela C.C.

##' @keywords internal
##' @rdname gev_internals
##' @export
new_to_old = function(par, alpha = 0.5, beta = 0.5){
    q = par[1]
    s = par[2]
    xi = par[3]
    if(xi == 0){
        ell1 = log(-log(alpha))
        ell2 = log(-log(beta/2))
        ell3 = log(-log(1-beta/2))
    }else{
        ell1 = (-log(alpha))^(-xi)
        ell2 = (-log(beta/2))^(-xi)
        ell3 = (-log(1-beta/2))^(-xi)
    }
    if(xi == 0){
        sigma = s/(ell2 - ell3)
        mu    = q + sigma*ell1
    }else{
        mu    = q-s*(ell1 - 1)/(ell3-ell2)
        sigma = xi*s/(ell3-ell2)
    }
    list(mu = mu, sigma = sigma, xi = xi)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
old_to_new = function(par, alpha = 0.5, beta = 0.5){
    mu    = par[1]
    sigma = par[2]
    xi    = par[3]
    qalpha = qgev(alpha, mu, sigma, xi)
    qbeta1 = qgev(beta/2, mu, sigma, xi)
    qbeta2 = qgev((1-beta/2), mu, sigma, xi)
    list(q = qalpha, s = qbeta2 - qbeta1, xi = xi)
}

##' @title Fix lengths of variables
##'
##' @description Fix lengths of variables by repeating them to the
##' length of the longest variable.
##'
##' @details This function takes a list of variable names and repeats
##' them to the length of the longest variable. If the length of the
##' longest variable is not a multiple of the length of the other
##' variables, an error is thrown. The variables are modified in place.
##'
##' @param ... A list of variable names.
##'
##' @return The function returns 0 invisibly.
##' @author Daniela Castro-Camilo
##'
##' @keywords internal
##' @export
fix_lengths <- function(...) {
    call = match.call()
    varnames = sapply(call[-1], as.character)
    e = parent.frame()
    vars = lapply(varnames, get, envir = e)
    lengths = sapply(vars, length)
    max_length = max(lengths)
    if (any(max_length %% lengths != 0)) stop("Bad input lengths")
    for (i in seq_along(vars)) {
        if (lengths[i] < max_length) {
            assign(varnames[i], rep(vars[[i]], max_length / lengths[i]), envir = e)
        }
    }
    0
}

## END Copy of bGEV/Code/utils.R
##==============================================================================

##==============================================================================
## Copy of bGEV/Code/bGEVcode.R

##################################################
## Functions to work with the bGEV distribution ##
##################################################
# By Silius M.V. and Daniela C.C.

##' @rdname gev_internals
##' @keywords internal
##' @export
##' @importFrom stats pbeta
pbgev = function(x, mu, sigma, xi, p_a = .1, p_b = .2, s = 5) {
    # PDF
    fix_lengths(x, mu, sigma, xi, p_a, p_b, s)
    g = get_gumbel_par(mu, sigma, xi, p_a, p_b)
    a = qgev(p_a, mu, sigma, xi)
    b = qgev(p_b, mu, sigma, xi)
    p = pbeta((x - a) / (b - a), s, s)
    pgev(x, mu, sigma, xi) ^ p * pgev(x, g$mu, g$sigma, 0) ^ (1 - p)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
qbgev = function(p, mu, sigma, xi, p_a = .1, p_b = .2, s = 5) {
    # Quantile function
    fix_lengths(p, mu, sigma, xi, p_a, p_b, s)
    res = rep(NA, length(p))
    gumbel = which(p <= p_a)
    frechet = which(p >= p_b)
    mixing = which(p_a < p & p < p_b)
    if (any(gumbel)) {
        g = get_gumbel_par(mu[gumbel], sigma[gumbel], xi[gumbel],
            p_a[gumbel], p_b[gumbel])
        res[gumbel] = qgev(p[gumbel], g$mu, g$sigma, 0)
    }
    if (any(frechet)) {
        res[frechet] = qgev(p[frechet], mu[frechet], sigma[frechet],
            xi[frechet])
    }
    if (any(mixing)) {
        res[mixing] = qbgev_mixing(p[mixing], mu[mixing], sigma[mixing],
            xi[mixing], p_a[mixing], p_b[mixing], s[mixing])
    }
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
##' @importFrom stats runif
rbgev = function(n, mu, sigma, xi, p_a = .1, p_b = .2, s = 5) {
    # Random generation
    lengths = sapply(list(mu, sigma, xi), length)
    if (any(lengths > n)) stop("Bad input lengths")
    qbgev(runif(n), mu, sigma, xi, p_a, p_b, s)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
dbgev = function(x, mu, sigma, xi, p_a = .1, p_b = .2, s = 5, log = FALSE) {
    # Density using the usual parametrisation
    fix_lengths(x, mu, sigma, xi, p_a, p_b, s)
    a = qgev(p_a, mu, sigma, xi)
    b = qgev(p_b, mu, sigma, xi)
    res = rep(NA, length(x))
    gumbel = which(x <= a)
    frechet = which(x >= b)
    mixing = which(a < x & x < b)
    if (any(gumbel)) {
        g = get_gumbel_par(mu[gumbel], sigma[gumbel], xi[gumbel],
            p_a[gumbel], p_b[gumbel])
        res[gumbel] = dgev(x[gumbel], g$mu, g$sigma, 0, log = log)
    }
    if (any(frechet)) {
        res[frechet] = dgev(x[frechet], mu[frechet], sigma[frechet],
            xi[frechet], log = log)
    }
    if (any(mixing)) {
        res[mixing] = dbgev_mixing(x[mixing], mu[mixing], sigma[mixing],
            xi[mixing], p_a[mixing], p_b[mixing], s[mixing], log = log)
    }
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
dbgev2 = function(x, q, sb, xi, alpha = 0.5, beta = 0.5, p_a = .1, p_b = .2,
                  s = 5, log = FALSE) {
    # Density using the new parametrisation
    fix_lengths(x, q, sb, xi, p_a, p_b, s)
    tmp     = new_to_old(c(q,sb,xi), alpha = alpha, beta = beta)
    mu      = tmp$mu
    sigma   = tmp$sigma
    a       = qgev(p_a, mu, sigma, xi)
    b       = qgev(p_b, mu, sigma, xi)
    res     = rep(NA, length(x))
    gumbel  = which(x <= a)
    frechet = which(x >= b)
    mixing  = which(a < x & x < b)
    if (any(gumbel)) {
        g = get_gumbel_par(mu[gumbel], sigma[gumbel], xi[gumbel],
            p_a[gumbel], p_b[gumbel])
        res[gumbel] = dgev(x[gumbel], g$mu, g$sigma, 0, log = log)
    }
    if (any(frechet)) {
        res[frechet] = dgev(x[frechet], mu[frechet], sigma[frechet],
            xi[frechet], log = log)
    }
    if (any(mixing)) {
        res[mixing] = dbgev_mixing(x[mixing], mu[mixing], sigma[mixing],
            xi[mixing], p_a[mixing], p_b[mixing], s[mixing], log = log)
    }
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
return_level_bgev = function(period, mu, sigma, xi, p_a = .1, p_b = .2,
                             s = 5) {
    # Return levels
    if (any(period <= 1)) warning("invalid period")
    p = ifelse(period > 1, 1 - 1 / period, NA)
    qbgev(p, mu, sigma, xi, p_a, p_b, s)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
##' @importFrom stats pbeta dbeta
dbgev_mixing = function(x, mu, sigma, xi, p_a = .1, p_b = .2,
                        s = 5, log = FALSE) {
    # Tool function for dgev
    g = get_gumbel_par(mu, sigma, xi, p_a, p_b)
    a = qgev(p_a, mu, sigma, xi)
    b = qgev(p_b, mu, sigma, xi)
    if (any(x <= a | x >= b)) stop("x is outside the domain for mixing")
    p = pbeta((x - a) / (b - a), s, s)
    p_der = dbeta((x - a) / (b - a), s, s) / (b - a)
    term1 = - p_der * (1 + xi * (x - mu) / sigma) ^ (-1 / xi)
    term2 = p / sigma * (1 + xi * (x - mu) / sigma) ^ (-1 / xi - 1)
    term3 = p_der * exp(- (x - g$mu) / g$sigma)
    term4 = (1 - p) / g$sigma * exp(- (x - g$mu) / g$sigma)
    term0 = p * log(pgev(x, mu, sigma, xi)) +
        (1 - p) * log(pgev(x, g$mu, g$sigma, 0))
    res = term0 + log(term1 + term2 + term3 + term4)
    if (!log) res = exp(res)
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
##' @importFrom stats uniroot
qbgev_mixing = function(p, mu, sigma, xi, p_a = .1, p_b = .2, s = 5,
                        lower = 0, upper = 100) {
    # Tool function for qgev
    if (any(p <= p_a | p >= p_b)) stop("p is outside the domain for mixing")
    res = vector("numeric", length(p))
    for (i in seq_along(p)) {
        f = function(x) (pbgev(x, mu, sigma, xi, p_a, p_b, s) - p)[i]
        sol = uniroot(f, lower = lower, upper = upper, extendInt = "upX")
        res[i] = sol$root
    }
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
get_gumbel_par = function(mu, sigma, xi, p_a = .1, p_b = .2) {
    # Tool function to get parameters for G (Gumbel)
    if (any(xi < 0)) stop("xi must be nonnegative")
    a = qgev(p_a, mu, sigma, xi)
    b = qgev(p_b, mu, sigma, xi)
    sigma2 = (b - a) / log(log(p_a) / log(p_b))
    mu2 = a + sigma2 * log(-log(p_a))
    list(mu = mu2, sigma = sigma2)
}

## END Copy of bGEV/Code/bGEVcode.R
##==============================================================================

##==============================================================================
## Copy of bGEV/Code/GEVcode.R

#################################################
## Functions to work with the GEV distribution ##
#################################################
# By Silius M.V. and Daniela C.C.

##' @rdname gev_internals
##' @keywords internal
##' @export
pgev = function(x, mu, sigma, xi) {
    # PDF
    fix_lengths(x, mu, sigma, xi)
    ifelse(xi == 0,
        exp(-exp(- (x - mu) / sigma)),
        exp(-pmax(0, 1 + xi * (x - mu) / sigma) ^ (-1 / xi)))
}

##' @rdname gev_internals
##' @keywords internal
##' @export
qgev = function(p, mu, sigma, xi) {
    # Quantile function
    fix_lengths(p, mu, sigma, xi)
    ifelse(xi == 0,
        mu - sigma * log(-log(p)),
        ## mu - sigma * (1 / xi) * (1 - (- log(1 - p)) ^ (-xi)))
        mu - sigma * (1 / xi) * (1 - (- log(p)) ^ (-xi)))
}

##' @rdname gev_internals
##' @export
##' @keywords internal
##' @importFrom stats runif
rgev = function(n, mu, sigma, xi) {
    # Random generation
    lengths = sapply(list(mu, sigma, xi), length)
    if (any(lengths > n)) stop("Bad input lengths")
    qgev(runif(n), mu, sigma, xi)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
dgev = function(x, mu, sigma, xi, log = FALSE) {
    # Density using the usual parametrisation
    fix_lengths(x, mu, sigma, xi)
    res = ifelse(xi == 0,
        -exp(- (x - mu) / sigma),
        -pmax(0, 1 + xi * (x - mu) / sigma) ^ (-1 / xi))
    res = res - log(sigma) +
        ifelse(xi == 0,
            - (x - mu) / sigma,
        ifelse(1 + xi * (x - mu) / sigma > 0,
            - (1 / xi + 1) * log(1 + xi * (x - mu) / sigma),
            -Inf))
    if (!log) res = exp(res)
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
dgev2 = function(x, q, sb, xi, alpha = 0.5, beta = 0.5, log = FALSE) {
    # Density using the new parametrisation
    tmp   = new_to_old(c(q,sb,xi), alpha = alpha, beta = beta)
    mu    = tmp$mu
    sigma = tmp$sigma
    res = ifelse(xi == 0,
        -exp(- (x - mu) / sigma),
        -pmax(0, 1 + xi * (x - mu) / sigma) ^ (-1 / xi))
    res = res - log(sigma) +
        ifelse(xi == 0,
            - (x - mu) / sigma,
        ifelse(1 + xi * (x - mu) / sigma > 0,
            - (1 / xi + 1) * log(1 + xi * (x - mu) / sigma),
            -Inf))
    if (!log) res = exp(res)
    res
}

##' @rdname gev_internals
##' @keywords internal
##' @export
return_level_gev = function(period, mu, sigma, xi) {
    # Return levels
    if (any(period <= 1)) warning("invalid period")
    p = ifelse(period > 1, 1 - 1 / period, NA)
    ## p = ifelse(period > 1, 1 / period, NA)
    qgev(p, mu, sigma, xi)
}

## END Copy of bGEV/Code/GEVcode.R
##==============================================================================

##==============================================================================
## Copy of bGEV/Code/GEVbGEVlikelihoods.R


#############################################################################
## Negative log-likelihood functions associates to the GEV and bGEV models ##
#############################################################################
# By Daniela C.C.

##' @rdname gev_internals
##' @keywords internal
##' @export
nllik_gev = function(par, x, log = TRUE){
    # Neg log-lik of GEV using classical parametrisation
    mu    = par[1]
    sigma = par[2]
    xi    = par[3]
    ll    = rep(NA, length(x))
    for(i in 1:length(x))
        ll[i] = dgev(x[i], mu, sigma, xi, log = log)
    -sum(ll)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
nllik_bgev = function(par, x, p_a, p_b, s, log = TRUE){
    # Neg log-lik of bGEV using classical parametrisation
    mu    = par[1]
    sigma = par[2]
    xi    = par[3]
    ll    = rep(NA, length(x))
    if(xi < 0) {
        return(1e10)
    } else {
        for(i in 1:length(x))
            ll[i] = dbgev(x[i], mu, sigma, xi, p_a = p_a, p_b = p_b,
                s = s, log = log)
        return(-sum(ll))
    }
}

##' @rdname gev_internals
##' @keywords internal
##' @export
nllik_gevx = function(par, x, w, log = TRUE){
    # Neg log-lik of GEV using classical parametrisation with
    # covariate-dependent location
    mu0   = par[1]
    mu1   = par[2]
    sigma = par[3]
    xi    = par[4]
    mu    = mu0 + mu1*w
    ll    = rep(NA, length(x))
    for(i in 1:length(x))
        ll[i] = dgev(x[i], mu[i], sigma, xi, log = log)
    -sum(ll)
}

##' @rdname gev_internals
##' @keywords internal
##' @export
nllik_bgevx = function(par, x, w, p_a, p_b, s, log = TRUE){
    # Neg log-lik of bGEV using classical parametrisation with
    # covariate-dependent location
    mu0   = par[1]
    mu1   = par[2]
    sigma = par[3]
    xi    = par[4]
    mu    = mu0 + mu1*w
    ll    = rep(NA, length(x))
    if(xi < 0) {
        return(1e10)
    } else {
        for(i in 1:length(x))
            ll[i] = dbgev(x[i], mu[i], sigma, xi, p_a = p_a, p_b = p_b,
                s = s, log = log)
        return(-sum(ll))
    }
}

##' @rdname gev_internals
##' @keywords internal
##' @export
nllik_gev2 = function(par, x, alpha = 0.5, beta = 0.5, log = TRUE){
    # Neg log-lik of GEV using new parametrisation
    q  = par[1]
    s  = par[2]
    xi = par[3]
    ll = rep(NA, length(x))
    if(xi < 0) {
        return(1e10)
    } else {
        for(i in 1:length(x))
            ll[i] = dgev2(x[i], q = q, sb = s, xi = xi, alpha = alpha,
                beta = beta, log = log)
        return(-sum(ll))
    }
}

##' @rdname gev_internals
##' @keywords internal
##' @export
nllik_bgev2 = function(par, x, alpha = 0.5, beta = 0.5, p_a, p_b,
                       s, log = TRUE){
    # Neg   log-lik of bGEV using new parametrisation
    tmp   = new_to_old(par, alpha = alpha, beta = beta)
    mu    = tmp$mu
    sigma = tmp$sigma
    ll    = rep(NA, length(x))
    if(xi < 0) {
        return(1e10)
    } else {
        for(i in 1:length(x))
            ll[i] = dbgev(x[i], mu, sigma, xi, p_a = p_a, p_b = p_b,
                s = s, log = log)
        return(-sum(ll))
    }
}

## END Copy of bGEV/Code/GEVbGEVlikelihoods.R
##==============================================================================


##==============================================================================
## Copy of bGEV/Pollution_California/utilsBakersfield.R

##' @rdname gev_internals
##' @keywords internal
##' @export
map_tail = function(x, interval, inverse = FALSE) {
    ## Utility function to define hyper for tail
    if (!inverse) {
        return (
            interval[1] + (interval[2] - interval[1]) * exp(x)/(1.0 + exp(x))
        )
    } else {
        return (
            log((x-interval[1])/(interval[2]-x))
        )
    }
}

## END Copy of bGEV/Pollution_California/utilsBakersfield.R
##==============================================================================
