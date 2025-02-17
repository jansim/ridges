#' Export individual SVG files for each ridgeline
#'
#' Creates separate SVG files for each ridgeline slice, maintaining the same scaling
#' and proportions as plot_ridgelines().
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param output_dir Directory where SVG files will be saved (default: "export")
#' @param n_lines Number of ridgelines to generate (default: 30)
#' @param scale_factor Scaling factor for the height of ridgelines (default: 10)
#' @param line_color Color of the ridgelines (default: "black")
#' @param fill_color Color to fill closed shapes (default: same as line_color). Only used when closed = TRUE
#' @param linewidth Width of the ridgelines (default: 0.5)
#' @param width SVG width in pixels (default: 800)
#' @param height SVG height in pixels (default: 200)
#' @param closed Logical; if TRUE, creates closed shapes by adding a baseline (default: FALSE)
#' @return Invisibly returns the paths to the created SVG files
#' @export
#'
export_ridgeline_svgs <- function(elevation = NULL,
                                  output_dir = "export",
                                  n_lines = 30,
                                  scale_factor = 10,
                                  line_color = "black",
                                  fill_color = NULL,
                                  linewidth = 0.5,
                                  width = 800,
                                  height = 200,
                                  closed = TRUE) {
  # Check for invalid fill_color usage
  if (!is.null(fill_color) && !closed) {
    stop("fill_color can only be specified when closed = TRUE")
  }

  # If fill_color is NULL, use line_color
  fill_color <- if (is.null(fill_color)) "transparent" else fill_color

  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # Calculate ridgeline data
  plot_data <- calculate_ridgelines(elevation, n_lines)
  max_elevation <- max(plot_data$elevation)

  # Get the CRS from the elevation raster
  elevation_crs <- sf::st_crs(elevation)

  # Calculate the geographic distance between lines (same as in plot_ridgelines)
  y_coords <- unique(plot_data$y)[1:2]
  x_mid <- mean(plot_data$x)
  line_spacing <- sf::st_distance(
    sf::st_point(c(x_mid, y_coords[1])) |> sf::st_sfc(crs = elevation_crs),
    sf::st_point(c(x_mid, y_coords[2])) |> sf::st_sfc(crs = elevation_crs)
  )

  # Adjust scale factor based on geographic distance
  adjusted_scale <- scale_factor / as.numeric(line_spacing)

  # Create vector to store file paths
  svg_files <- character(n_lines)

  # Generate individual plots for each ridgeline
  for (i in 1:n_lines) {
    # Filter data for current ridgeline
    line_data <- plot_data[plot_data$group == i, ]

    # Create the plot
    if (closed) {
      # For closed shapes, use geom_polygon with a baseline
      baseline_data <- line_data
      baseline_data$elevation <- min(line_data$elevation)
      plot_data_closed <- rbind(line_data, baseline_data[nrow(baseline_data):1, ])

      p <- ggplot2::ggplot(
        plot_data_closed,
        ggplot2::aes(
          x = x,
          y = (elevation / max_elevation) * adjusted_scale
        )
      ) +
        ggplot2::geom_polygon(
          color = line_color,
          fill = fill_color,
          linewidth = linewidth
        )
    } else {
      p <- ggplot2::ggplot(
        line_data,
        ggplot2::aes(
          x = x,
          y = (elevation / max_elevation) * adjusted_scale
        )
      ) +
        ggplot2::geom_line(
          color = line_color,
          linewidth = linewidth
        )
    }

    p <- p +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::unit(c(0, 0, 0, 0), "pt")
      ) +
      ggplot2::coord_cartesian(
        expand = FALSE
      )

    # Generate filename with padding zeros
    filename <- file.path(output_dir, sprintf("ridgeline_%03d.svg", i))
    svg_files[i] <- filename

    # Save the plot
    ggplot2::ggsave(
      filename,
      p,
      width = width / 72, # Convert pixels to inches
      height = height / 72,
      dpi = 72,
      bg = "transparent"
    )
  }

  # Return file paths invisibly
  invisible(svg_files)
}
