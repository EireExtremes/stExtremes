##' @title Animate the model output for a given field and barrier
##'
##' @description This function animates the model output for a given
##' field and barrier. The field is a list of matrices, each matrix is a
##' 2D array of the field values at a given year. The barrier is a sf
##' object that represents the barrier. The grid is a list of x and y
##' values that represent the grid points. The data is a sf object that
##' is used to plot the points. The name is the name of the output file.
##'
##' @details The function uses the gganimate package to animate the
##' model output.
##'
##' @param data A sf object that represents the data points to be
##' plotted on the map.
##' @param barrier A sf object that represents the barrier.
##' @param grid A list of x and y values that represent the grid points.
##' @param field A list of matrices, each matrix is a 2D array of the
##' field values at a given year.
##' @param crs The Coordinate Reference System of the data points
##'
##' @return A gif file that represents the animation of the model output.
##' @author Fernando Mayer
##'
##' @import ggplot2 sf dplyr
##' @importFrom tidyr pivot_longer
##' @importFrom viridis scale_colour_viridis
##' @export
field_plot <- function(data, barrier, grid, field, crs) {
    ## Get the year range
    yrs <- range(data$year)
    tmp <- expand.grid(x = grid$x, y = grid$y)
    zzz <- map(field, as.vector)
    names(zzz) <- paste0("t", yrs[1]:yrs[2])
    tmp <- bind_cols(tmp, zzz)
    time <- names(zzz)
    ## Transform to long format
    tmp <- tidyr::pivot_longer(
                      tmp, cols = starts_with("t"),
                      names_to = "time",
                      values_to = "value") |>
        mutate(time = as.integer(gsub("t", "", time)))
    tmp <- st_as_sf(tmp, coords = c("x", "y"), crs = crs)
    fplot <- ggplot(tmp) +
        geom_sf(aes(color = value)) +
        viridis::scale_colour_viridis() +
        geom_sf(data = barrier, fill = "grey") +
        theme(plot.title = element_text(size = 20, face = "bold"),
            axis.text = element_text(size = 14),
            legend.text = element_text(size = 12)) +
        theme_void() +
        facet_wrap(~ time)
        ## coord_sf(datum = crs)
    return(fplot)
}

## anim_field <- function(plot, name, time, ...) {
##     anim <- plot +
##         transition_time(time) +
##         labs(title = 'Year: {frame_time}', x = "", y = "",
##             fill = "") +
##         shadow_wake(wake_length = 0.1, alpha = FALSE) +
##         ease_aes('cubic-in')
##     if(!dir.exists("animation")) {
##         dir.create("animation")
##     }
##     fln <- paste0("animation/anim-", name, ".gif")
##     animate(
##         anim, width = 800, height = 800, units = "px", fps = 100,
##         renderer = gifski_renderer(fln), ...
##     )
## }
