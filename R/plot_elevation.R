#' Plot elevation data as a heatmap
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param title Optional title for the plot
#' @param low_color Color for lowest elevations (default: "darkblue")
#' @param high_color Color for highest elevations (default: "white")
#' @return A ggplot object
#' @export
#'
plot_elevation <- function(elevation = NULL,
                          title = "Elevation Heatmap",
                          low_color = "darkblue",
                          high_color = "white") {
  # Use last elevation if none provided
  if (is.null(elevation)) {
    if (is.null(.last_elevation)) {
      stop("No elevation data provided and no last elevation data available")
    }
    elevation <- .last_elevation
  }

  # Convert raster to data frame for ggplot
  elevation_df <- terra::as.data.frame(elevation, xy = TRUE)
  colnames(elevation_df)[3] <- "elevation"

  # Create the plot
  ggplot2::ggplot(elevation_df, ggplot2::aes(x = x, y = y, fill = elevation)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient(
      low = low_color,
      high = high_color,
      name = "Elevation (m)"
    ) +
    ggplot2::coord_equal() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      x = "Longitude",
      y = "Latitude"
    )
}