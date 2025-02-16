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
#' @param linewidth Width of the ridgelines (default: 0.5)
#' @return A ggplot object
#' @export
#'
plot_ridgelines <- function(elevation = NULL,
                            n_lines = 30,
                            scale_factor = 10,
                            line_color = "#000000",
                            fill_color = "#0000001A",
                            linewidth = 0.5) {
  # Calculate ridgeline data
  plot_data <- calculate_ridgelines(elevation, n_lines)
  max_elevation <- max(plot_data$elevation)

  # Get the CRS from the elevation raster
  elevation_crs <- sf::st_crs(elevation)

  # Calculate the geographic distance between lines
  y_coords <- unique(plot_data$y)[1:2]
  x_mid <- mean(plot_data$x)
  line_spacing <- sf::st_distance(
    sf::st_point(c(x_mid, y_coords[1])) |> sf::st_sfc(crs = elevation_crs),
    sf::st_point(c(x_mid, y_coords[2])) |> sf::st_sfc(crs = elevation_crs)
  )

  # Adjust scale factor based on geographic distance
  adjusted_scale <- scale_factor / as.numeric(line_spacing)

  # Create the plot
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = x,
      y = y,
      height = (elevation / max_elevation) * adjusted_scale,
      group = group
    )
  ) +
    ggridges::geom_ridgeline(
      color = line_color,
      fill = fill_color,
      linewidth = linewidth,
      show.legend = FALSE
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::coord_sf(crs = elevation_crs) +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    )
}
