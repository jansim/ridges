.last_elevation <- NULL

#' Get elevation data for a bounding box
#'
#' Retrieves elevation data for a specified geographic area using the elevatr package.
#' The data is returned as a raster object that can be used with the plotting
#' functions in this package.
#'
#' @param bb A bounding box object (sf::bbox) or NULL to use the last drawn bounding box
#' @param z Zoom level for elevation data (1-14, higher means more detail). Default is 9,
#'   which provides a good balance between detail and download size.
#' @return A raster object containing elevation data
#' @export
#'
#' @examples
#' # Using the included Wilder Kaiser bounding box
#' elevation <- get_elevation(bb_wilder_kaiser)
#' plot_elevation()
#'
#' # Using a higher zoom level for more detail
#' detailed_elevation <- get_elevation(bb_wilder_kaiser, z = 12)
#' plot_elevation()
#'
get_elevation <- function(bb = NULL, z = 9) {
  # Use last drawn bounding box if none provided
  if (is.null(bb)) {
    if (is.null(get_last_bb())) {
      stop("No bounding box available. Please run draw_bb() first.")
    }
    bb <- get_last_bb()
  }

  # Validate and normalize longitude to -180 to 180 range
  bb_vec <- unclass(bb)
  bb_vec["xmin"] <- ((bb_vec["xmin"] + 180) %% 360) - 180
  bb_vec["xmax"] <- ((bb_vec["xmax"] + 180) %% 360) - 180
  bb <- sf::st_bbox(bb_vec)

  # Convert bbox to spatial object as required by get_elev_raster
  bbox_polygon <- sf::st_bbox(bb) |>
    sf::st_as_sfc() |>
    sf::st_sf(crs = 4326)

  # Get elevation data
  elevation <- elevatr::get_elev_raster(
    locations = bbox_polygon,
    z = z,
    clip = "locations"
  )

  # Save last elevation data for potential later use
  set_last_ele(elevation)

  return(elevation)
}
