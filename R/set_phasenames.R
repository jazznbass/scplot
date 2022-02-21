#' Set phasenames of an scplot
#'
#' @inheritParams .inherit_scplot
#' @position Character string either 'left' or 'center'.
#' @export
set_phasenames <- function(object, labels = NULL,
                           position = NULL,
                           ...) {

  args_text <- list(...)
  if (!is.null(args_text$size)) args_text$size <- rel(args_text$size)

  if (!is.null(labels)) object$phasenames$labels <- labels
  if (!is.null(position)) object$theme$phasenames.position.x <- position

  object$theme$phasenames <- .merge_element(args_text, object$theme$phasenames)

  object
}
