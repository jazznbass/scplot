#' Set base text parameters of an scplot
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
