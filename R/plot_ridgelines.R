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
                           n_lines = 20,
                           scale_factor = 1,
                           line_color = "black",
                           fill_color = "black") {
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
  y_seq <- seq(from = min(elevation_df$y),
               to = max(elevation_df$y),
               length.out = n_lines)

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
  plot_data <- do.call(rbind, ridgeline_data)

  # Create the plot
  ggplot2::ggplot(plot_data,
                  ggplot2::aes(x = x,
                              y = group,
                              height = elevation * scale_factor,
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