#' Set casenames of an scplot
#'
#' @inheritParams .inherit_scplot
#' @param position Either "strip", "topleft", "bottomleft", "topright", "bottomright", or a numerical vector of length 2 with the x and y position (e.g. c(19, 20)).
#' @export
set_casenames <- function(object, labels = NULL,
                          position = NULL,
                          background = list(),
                          ...) {

  args_text <- list(...)
  args_rect <- background

  if (!is.null(labels)) object$casenames$labels <- labels

  if (!is.null(position)) object$theme$casenames.position <- position
  if (!is.null(args_text$size)) args_text$size <- rel(args_text$size)

  if (identical(position, "strip")) {
    if (is.null(args_text$angle)) args_text$angle <- 270
    if (is.null(args_text$hjust)) args_text$hjust <- 0.5
  }

  object$theme$casenames.strip <- .merge_element(
    args_rect, object$theme$casenames.strip)

  object$theme$casenames <- .merge_element(args_text, object$theme$casenames)

  object
}
