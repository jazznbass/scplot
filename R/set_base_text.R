#' Set base text parameters of an scplot object
#'
#' Sets the base text parameters for all text elements in an `scplot` object.
#'
#' This function modifies the `text` element in the `theme` list of the
#' `scplot` object, allowing you to define default text properties such as font
#' family, face, color, size, alignment, angle, line height, and margin
#' using parameters similar to those in `element_text()`.
#'
#' @inheritParams .inherit_scplot
#' @param ... List with text parameters (`"family", "face", "colour", "size",
#'   "hjust", "vjust", "angle", "lineheight", "margin"`). See [element_text()].
#' @return An object of class `scplot` (see[scplot()]).
#' @export
set_base_text <- function(object, ...) {

  args <- do.call("element_text", list(...))
  object$theme$text <- merge_element(args, object$theme$text)

  object
}
