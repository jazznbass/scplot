#' Set phasenames of an scplot
#'
#' @inheritParams .inherit_scplot
#' @param position Character string either `'left'`, `'center'`, or `'none'`.
#' @param ... List with text parameters (`"family", "face", "colour", "size",
#'   "hjust", "vjust", "angle", "lineheight", "margin"`). See [element_text()].
#' @return An object of class `scplot` (see [scplot()]) with a changed
#'   `phasenames` element.
#' @export
set_phasenames <- function(object,
                           labels = NULL,
                           position = NULL,
                           ...) {

  args_text <- list(...)
  if (!is.null(args_text$size)) args_text$size <- rel(args_text$size)
  if (!is.null(labels)) object$phasenames$labels <- labels
  if (!is.null(position)) object$theme$phasenames.position.x <- position

  object$theme$phasenames <- .merge_element(args_text, object$theme$phasenames)

  object
}
