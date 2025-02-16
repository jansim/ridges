#' Calculate ridgeline data from elevation
#'
#' Takes elevation data and calculates the data needed for plotting ridgelines
#' by sampling the elevation at regular y-intervals.
#'
#' @param elevation A raster object containing elevation data, or NULL to use last elevation
#' @param n_lines Number of ridgelines to calculate
#' @return A data frame containing the ridgeline data with columns x, y, elevation, and group
#' @keywords internal
#'
calculate_ridgelines <- function(elevation = NULL, n_lines = 30) {
  # Use last elevation if none provided
  if (is.null(elevation)) {
    if (is.null(.last_elevation)) {
      stop("No elevation data provided and no last elevation data available")
    }
    elevation <- .last_elevation
  }

  # Convert raster to data frame
  elevation_df <- terra::as.data.frame(elevation, xy = TRUE)
  colnames(elevation_df)[3] <- "elevation"

  # Create sequence of y positions for sampling
  y_seq <- seq(
    from = min(elevation_df$y),
    to = max(elevation_df$y),
    length.out = n_lines
  )

  # Create empty list to store ridgeline data
  ridgeline_data <- list()

  # Sample elevation data at each y position
  for (i in seq_along(y_seq)) {
    # Find closest y values
    closest_y <- which.min(abs(elevation_df$y - y_seq[i]))
    y_range <- elevation_df$y[closest_y]

    # Extract data for this y position
    line_data <- elevation_df[elevation_df$y == y_range, ]

    # Add to list with group identifier
    line_data$group <- i
    ridgeline_data[[i]] <- line_data
  }

  # Combine all data
  do.call(rbind, ridgeline_data)
}
