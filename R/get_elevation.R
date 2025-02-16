.last_elevation <- NULL

#' Get elevation data for a bounding box
#'
#' @param bb A bounding box object (sf::bbox) or NULL to use the last drawn bounding box
#' @param z Zoom level for elevation data (1-14, higher means more detail)
#' @return A raster object containing elevation data
#' @export
#'
get_elevation <- function(bb = NULL, z = 9) {
  # Use last drawn bounding box if none provided
  if (is.null(bb)) {
    if (is.null(.last_bb)) {
      stop("No bounding box provided and no last drawn bounding box available")
    }
    bb <- .last_bb
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
  .last_elevation <<- elevation

  return(elevation)
}
