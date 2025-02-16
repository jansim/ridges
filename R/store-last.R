.last_store <- function() {
  .last_plot <- NULL

  list(
    get = function() .last_plot,
    set = function(value) .last_plot <<- value
  )
}

.last_ele_store <- .last_store()
.last_bb_store <- .last_store()

#' Set the last elevation data
#'
#' @seealso [get_last_ele()]
#' @keywords internal
set_last_ele <- function(value) .last_ele_store$set(value)


#' Get the last elevation data
#'
#' @seealso [get_elevation()]
#' @export
#' @keywords internal
get_last_ele <- function() .last_ele_store$get()

#' Set the last bounding box data
#'
#' @seealso [get_last_bb()]
#' @keywords internal
set_last_bb <- function(value) .last_bb_store$set(value)

#' Get the last bounding box data
#'
#' @seealso [draw_bb()]
#' @export
#' @keywords internal
get_last_bb <- function() .last_bb_store$get()
