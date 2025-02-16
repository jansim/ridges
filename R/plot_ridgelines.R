#' Plot elevation data as ridgelines
#'
#' Creates a Joy Division style visualization of elevation data, showing elevation
#' profiles as stacked ridgelines.
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param n_lines Number of ridgelines to draw (default: 20)
#' @param scale_factor Scaling factor for the height of ridgelines (default: 1)
#' @param line_color Color of the ridgelines (default: "black")
#' @param fill_color Fill color below the lines (default: "black", with alpha = 0.1)
#' @return A ggplot object
#' @export
#'
plot_ridgelines <- function(elevation = NULL,
                           n_lines = 30,
                           scale_factor = 4,
                           line_color = "black",
                           fill_color = "black") {

  # Calculate ridgeline data
  plot_data <- calculate_ridgelines(elevation, n_lines)
  max_elevation <- max(plot_data$elevation)

  # Create the plot
  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = x,
                              y = group,
                              height = (elevation / max_elevation) * scale_factor,
                              group = group)) +
    ggridges::geom_ridgeline(color = line_color,
                            fill = scales::alpha(fill_color, 0.1),
                            show.legend = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Longitude",
      y = NULL
    )
}
