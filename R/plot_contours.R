#' Plot elevation data as contour lines
#'
#' Creates a visualization of elevation data using contour lines. This function provides
#' a way to visualize terrain elevation changes through isolines.
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param interval Elevation interval between contour lines in meters (default: 100)
#' @param line_color Color of the contour lines (default: "black")
#' @param linewidth Width of the contour lines (default: 0.25)
#' @param color_by_elevation How to color elevation: "none", "lines", "fill", or "both" (default: "none")
#' @param low_color Color for lowest elevations when coloring by elevation (default: "darkblue")
#' @param high_color Color for highest elevations when coloring by elevation (default: "darkred")
#' @param fill_alpha Alpha transparency for fill color (default: 1)
#' @return A ggplot object that can be further customized using ggplot2 functions
#' @export
#'
#' @examples
#' # Create a basic contour plot
#' plot_contours(ele_wilder_kaiser)
#'
#' # Customize contour interval and appearance
#' plot_contours(ele_wilder_kaiser,
#'   interval = 50,
#'   line_color = "darkred",
#'   linewidth = 0.1
#' )
#'
#' # Color contours by elevation
#' plot_contours(ele_wilder_kaiser, color_by_elevation = "lines")
#'
#' # Color fill by elevation
#' plot_contours(ele_wilder_kaiser, color_by_elevation = "fill")
#'
#' # Color both lines and fill by elevation
#' plot_contours(ele_wilder_kaiser, color_by_elevation = "both")
#'
plot_contours <- function(elevation = NULL,
                          interval = 100,
                          line_color = "black",
                          linewidth = 0.25,
                          color_by_elevation = "none",
                          low_color = "darkblue",
                          high_color = "darkred",
                          fill_alpha = 1) {
  # Validate color_by_elevation parameter
  if (!color_by_elevation %in% c("none", "lines", "fill", "both")) {
    stop('color_by_elevation must be one of: "none", "lines", "fill", "both"')
  }

  # Use last elevation if none provided
  if (is.null(elevation)) {
    if (is.null(get_last_ele())) {
      stop("No elevation data available. Please run get_elevation() first.")
    }
    elevation <- get_last_ele()
  }

  # Convert raster to data frame for ggplot
  elevation_df <- terra::as.data.frame(elevation, xy = TRUE)
  colnames(elevation_df)[3] <- "elevation"

  # Get the CRS from the elevation raster
  elevation_crs <- terra::crs(elevation)

  # Create the base plot
  p <- ggplot2::ggplot(elevation_df, ggplot2::aes(x = x, y = y, z = elevation))

  # Add filled contours if requested
  if (color_by_elevation %in% c("fill", "both")) {
    # Calculate breaks at the same interval as contour lines
    breaks <- seq(
      floor(min(elevation_df$elevation, na.rm = TRUE) / interval) * interval,
      ceiling(max(elevation_df$elevation, na.rm = TRUE) / interval) * interval,
      by = interval
    )

    p <- p + ggplot2::geom_contour_filled(
      ggplot2::aes(z = elevation),
      alpha = fill_alpha,
      breaks = breaks
    ) +
      ggplot2::scale_fill_manual(
        values = grDevices::colorRampPalette(c(low_color, high_color))(length(breaks) - 1),
        name = "Elevation (m)",
        guide = if (color_by_elevation == "both") "none" else "colorbar",
        labels = function(breaks) {
          # Create labels showing elevation ranges
          paste(utils::head(breaks, -1), utils::tail(breaks, -1), sep = "-")
        }
      )
  }

  # Add contour lines
  if (color_by_elevation %in% c("lines", "both")) {
    p <- p + ggplot2::geom_contour(
      ggplot2::aes(z = elevation, color = ggplot2::after_stat(level)),
      bins = 20,
      linewidth = linewidth,
      breaks = seq(
        floor(min(elevation_df$elevation, na.rm = TRUE) / interval) * interval,
        ceiling(max(elevation_df$elevation, na.rm = TRUE) / interval) * interval,
        by = interval
      )
    ) +
      ggplot2::scale_color_gradient(
        low = low_color,
        high = high_color,
        name = "Elevation (m)"
      )
  } else {
    p <- p + ggplot2::geom_contour(
      ggplot2::aes(z = elevation),
      bins = 20,
      color = line_color,
      linewidth = linewidth,
      breaks = seq(
        floor(min(elevation_df$elevation, na.rm = TRUE) / interval) * interval,
        ceiling(max(elevation_df$elevation, na.rm = TRUE) / interval) * interval,
        by = interval
      )
    )
  }

  # Add common plot elements
  p + ggplot2::coord_sf(crs = elevation_crs) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    )
}
