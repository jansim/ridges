# The last bounding box drawn by the user.
.last_bb <- NULL

#' Draw a bounding box on an interactive map in a web browser
#'
#' Opens an interactive map interface in your web browser where you can draw a rectangular
#' bounding box to select an area of interest. Use the rectangle tool in the top-right
#' corner to draw the box, then click 'Submit' to return the selection.
#'
#' @param start_place_name Optional address or place name to initially center the map on.
#'   This helps you quickly navigate to your area of interest.
#'
#' @return A bounding box object (sf::bbox) that can be used with get_elevation()
#' @export
#'
#' @examples
#' \dontrun{
#' # Open map centered on default location
#' bb <- draw_bb()
#'
#' # Open map centered on a specific location
#' bb_innsbruck <- draw_bb("Innsbruck, Austria")
#'
#' # Use the returned bounding box to get elevation data
#' elevation <- get_elevation(bb_innsbruck)
#' }
#
draw_bb <- function(start_place_name = NULL) {
  ui <- shiny::fluidPage(
    "Use the square button at the top-right to draw a rectangle bounding box, press 'Submit' to return the value.",
    leaflet::leafletOutput("map"),
    shiny::HTML("<br><br>"),
    shiny::tableOutput("boundingBox"),
    shiny::tableOutput("info"),
    shiny::actionButton("submit", "Submit", shiny::icon("paper-plane"), class = "btn btn-primary")
  )

  server <- function(input, output, session) {
    bb <- NULL
    output$boundingBox <- NULL
    output$info <- NULL

    on_feature_change <- function(feat) {
      coords <- unlist(feat$geometry$coordinates)
      coords <- matrix(coords, ncol = 2, byrow = TRUE)
      poly <- sf::st_sf(sf::st_sfc(sf::st_polygon(list(coords))), crs = 4326)

      # Safe bounding box for returning
      bb <<- sf::st_bbox(poly)

      # Safe as list for rendering in shiny
      bb_df <- as.data.frame(as.list(bb))
      output$boundingBox <- shiny::renderTable(bb_df)
      width <- bb_df$xmax - bb_df$xmin
      height <- bb_df$ymax - bb_df$ymin
      output$info <- shiny::renderTable(data.frame(
        aspectRatio = width / height,
        width = width,
        height = height
      ))
    }

    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet.extras::addDrawToolbar(
          position = "topright",
          singleFeature = TRUE,
          editOptions = leaflet.extras::editToolbarOptions(remove = FALSE),
          # Disable everything but rectangles
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          polygonOptions = FALSE
        )

      if (!is.null(start_place_name)) {
        start_bounds <- osmdata::getbb(start_place_name)
        if (sum(is.na(start_bounds)) == 0) {
          m <- m |> leaflet::fitBounds(start_bounds["x", "min"], start_bounds["y", "min"], start_bounds["x", "max"], start_bounds["y", "max"])
        } else {
          message(paste("Couldn't find place", start_place_name))
        }
      }

      return(m)
    })

    shiny::observeEvent(input$map_draw_new_feature, {
      feat <- input$map_draw_new_feature
      on_feature_change(feat)
    })

    shiny::observeEvent(input$map_draw_edited_features, {
      feat <- input$map_draw_edited_features$features[[1]]
      on_feature_change(feat)
    })

    # Stop app and return bounding box when clicking "Submit"
    shiny::observeEvent(input$submit, {
      set_last_bb(bb)

      shiny::stopApp(returnValue = bb)
    })
  }

  return(shiny::runApp(shiny::shinyApp(ui, server)))
}
