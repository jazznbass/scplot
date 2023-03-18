#' Set casenames of an scplot
#'
#' @inheritParams .inherit_scplot
#' @param position Either `"topleft", "bottomleft", "topright", "bottomright",
#'   "strip-right", "strip-top"`, or a numerical vector of length 2 with the x
#'   and y position (e.g. `c(19, 20)`).
#' @param ... List with text parameters (`"family", "face", "colour", "size",
#'   "hjust", "vjust", "angle", "lineheight", "margin"`). See [element_text()].
#' @return An object of class `scplot` (see[scplot()]) with a changed `casenames`
#'   element.
#' @export
set_casenames <- function(object,
                          labels = NULL,
                          position = NULL,
                          background = list(),
                          ...) {

  args_text <- list(...)
  args_rect <- background

  if (!is.null(labels)) object$casenames$labels <- labels

  if (!is.null(position)) object$theme$casenames.position <- position
  if (!is.null(args_text$size)) args_text$size <- rel(args_text$size)

  # backwards compatibility
  if (identical(object$theme$casenames.position, "strip")) {
    object$theme$casenames.position <- "strip-right"
  }

  if (identical(object$theme$casenames.position, "topright")) {
    if (is.null(args_text$hjust)) args_text$hjust <- 1
  }

  if (identical(object$theme$casenames.position, "bottomleft")) {
    if (is.null(args_text$vjust)) args_text$vjust <- 0
  }

  if (identical(object$theme$casenames.position, "bottomright")) {
    if (is.null(args_text$hjust)) args_text$hjust <- 1
    if (is.null(args_text$vjust)) args_text$vjust <- 0
  }

  if (identical(object$theme$casenames.position, "strip-right")) {
      if (is.null(args_text$angle)) args_text$angle <- 270
      if (is.null(args_text$hjust)) args_text$hjust <- 0.5
  }
  if (identical(object$theme$casenames.position, "strip-top")) {
    if (is.null(args_text$hjust)) args_text$hjust <- 0
  }

  object$theme$casenames.strip <- .merge_element(
    args_rect, object$theme$casenames.strip)
  object$theme$casenames.background <- .merge_element(
    args_rect, object$theme$casenames.background)

  object$theme$casenames <- .merge_element(args_text, object$theme$casenames)

  object
}
