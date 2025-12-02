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
##' @param name The name of the output file.
##'
##' @param crs The Coordinate Reference System of the data points
##' @return A gif file that represents the animation of the model output.
##' @author Fernando Mayer
##'
anim_plot <- function(data, barrier, grid, field, name, crs) {
    ## Get the year range
    yrs <- range(data$year)
    tmp <- expand.grid(x = grid$x, y = grid$y)
    zzz <- map(field, as.vector)
    names(zzz) <- paste0("t", yrs[1]:yrs[2])
    tmp <- bind_cols(tmp, zzz)
    time <- names(zzz)
    ## Transform to long format
    tmp <- pivot_longer(tmp, cols = starts_with("t"), names_to = "time",
        values_to = "value") |>
        mutate(time = as.integer(gsub("t", "", time)))
    tmp <- st_as_sf(tmp, coords = c("x", "y"), crs = crs)
    anim <- ggplot(tmp) +
        geom_sf(aes(color = value)) +
        viridis::scale_colour_viridis() +
        geom_sf(data = barrier, fill = "grey") +
        ## geom_sf(data = data, col = 2, size = 4) +
        ## theme(plot.title = element_text(size = 20, face = "bold"),
        ##     axis.text = element_text(size = 14),
        ##     legend.text = element_text(size = 12)) +
        theme(plot.title = element_text(size = 20, face = "bold"),
            legend.text = element_text(size = 12)) +
        coord_sf(datum = crs) +
        theme_void() +
        gganimate::transition_time(time) +
        labs(title = 'Year: {frame_time}', x = "", y = "",
            fill = "")
        ## gganimate::shadow_wake(wake_length = 0.1, alpha = FALSE) +
        ## gganimate::ease_aes('cubic-in')
    if(!dir.exists("animation")) {
        dir.create("animation")
    }
    fln <- paste0("animation/anim-", name, ".gif")
    ## future::plan("multisession", workers = 8L)
    gganimate::animate(
                   anim,
                   width = 480, height = 480, units = "px",
                   fps = 5, nframes = diff(yrs) + 1,
                   renderer = gganimate::gifski_renderer(fln)
               )
}
