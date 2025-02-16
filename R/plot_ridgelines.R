#' Plot elevation data as ridgelines
#'
#' Creates topographical elevation ridgeline plots, inspired by the iconic Joy Division
#' album cover.
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param n_lines Number of ridgelines to draw (default: 30). More lines create a denser
#'   visualization but may increase plotting time.
#' @param scale_factor Scaling factor for the height of ridgelines (default: 10). Higher
#'   values make the elevation differences more pronounced.
#' @param line_color Color of the ridgelines (default: "white")
#' @param fill_color Fill color below the lines (default: "#FFFFFF1A", semi-transparent white)
#' @param linewidth Width of the ridgelines (default: 0.5)
#' @param background_color Background color of the plot (default: "black")
#' @return A ggplot object that can be further customized using ggplot2 functions
#' @export
#'
#' @examples
#' # Default style (white on black)
#' plot_ridgelines(ele_wilder_kaiser)
#'
#' # No fill, just lines
#' plot_ridgelines(
#'   ele_wilder_kaiser,
#'   fill_color = NA,
#'   scale_factor = 12
#' )
#'
#' # Classic black on white style
#' plot_ridgelines(
#'   ele_wilder_kaiser,
#'   line_color = "#000000",
#'   fill_color = "white",
#'   background_color = "white",
#'   scale_factor = 8
#' )
#'
#' # Get creative!
#' plot_ridgelines(
#'   ele_wilder_kaiser,
#'   n_lines = 35,
#'   line_color = "#FF4081",
#'   fill_color = "#FF408133",
#'   background_color = "#1A237E"
#' )
#'
plot_ridgelines <- function(elevation = NULL,
                            n_lines = 30,
                            scale_factor = 10,
                            line_color = "white",
                            fill_color = "#FFFFFF1A",
                            linewidth = 0.5,
                            background_color = "black") {
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
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = background_color, color = NA),
      panel.background = ggplot2::element_rect(fill = background_color, color = NA),
      axis.text = ggplot2::element_text(color = line_color),
      axis.title = ggplot2::element_text(color = line_color)
    ) +
    ggplot2::coord_sf(crs = elevation_crs) +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    )
}
