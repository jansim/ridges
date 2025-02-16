#' Plot elevation data as 3D ridgelines using rayshader
#'
#' Creates 3D topographical elevation ridgeline plots using rayshader,
#' providing an interactive 3D visualization of the terrain.
#'
#' @param elevation A raster object containing elevation data, as returned by get_elevation()
#' @param n_lines Number of ridgelines to draw (default: 30)
#' @param line_color Color of the ridgelines (default: "white")
#' @param background_color Background color of the plot (default: "black")
#' @param zoom Initial zoom level (default: 0.7)
#' @param phi Initial phi angle for viewing (default: 30)
#' @param theta Initial theta angle for viewing (default: 60)
#' @param windowsize Size of the plotting window in pixels (default: 800)
#' @param scale_factor Scale factor for elevation (default: 10)
#' @return Invisibly returns NULL, displays an interactive 3D plot
#' @export
#'
#' @examples
#' # Default style (white on black)
#' plot_ridgelines_3d(ele_wilder_kaiser)
#'
#' # Customize colors and viewing angle
#' plot_ridgelines_3d(
#'   ele_wilder_kaiser,
#'   line_color = "#FF4081",
#'   background_color = "#1A237E",
#'   phi = 35,
#'   theta = 60
#' )
#'
plot_ridgelines_3d <- function(elevation = NULL,
                               n_lines = 30,
                               line_color = "white",
                               background_color = "black",
                               zoom = 0.7,
                               phi = -65,
                               theta = 0,
                               windowsize = 800,
                               scale_factor = 10) {
  # Check if rgl is installed
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop(
      "Package 'rgl' is required for 3D plotting.\n",
      "Please install it with install.packages('rgl')"
    )
  }

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

  # Convert the data to a format suitable for rayshader
  # Split the data by group and convert to a list of matrices
  matrices <- lapply(split(plot_data, plot_data$group), function(group_data) {
    # Create matrix with x, y, z coordinates, applying the scaling
    scaled_elevation <- (group_data$elevation / max_elevation) * adjusted_scale
    matrix(c(group_data$x, group_data$y, scaled_elevation),
      ncol = 3
    )
  })

  # Set up the 3D plotting window
  rgl::clear3d()
  rgl::bg3d(color = background_color)

  # Plot each ridgeline
  for (mat in matrices) {
    # Draw the line
    rgl::lines3d(mat[, 1], mat[, 2], mat[, 3],
      color = line_color,
      lwd = 2
    )
  }

  # Set the viewing parameters
  rgl::view3d(theta = theta, phi = phi, zoom = zoom)

  # Set window size
  rgl::par3d(windowRect = c(20, 20, windowsize, windowsize))

  invisible(NULL)
}
