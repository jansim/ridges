#' Plot elevation data as a heatmap
#'
#' Creates a heatmap visualization of elevation data. This function provides a
#' quick way to visualize the terrain in your area of interest.
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param low_color Color for lowest elevations (default: "darkblue")
#' @param high_color Color for highest elevations (default: "white")
#' @return A ggplot object that can be further customized using ggplot2 functions
#' @export
#'
#' @examples
#' # Create a basic elevation plot
#' plot_elevation(ele_wilder_kaiser)
#'
#' # Customize colors
#' plot_elevation(ele_wilder_kaiser, low_color = "darkgreen", high_color = "yellow")
#'
plot_elevation <- function(elevation = NULL,
                           low_color = "darkblue",
                           high_color = "white") {
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

  # Create the plot
  ggplot2::ggplot(elevation_df, ggplot2::aes(x = x, y = y, fill = elevation)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradient(
      low = low_color,
      high = high_color,
      name = "Elevation (m)"
    ) +
    ggplot2::coord_sf(crs = elevation_crs) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude"
    )
}
