#' Add title and caption to an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
add_title <- function(object, label, ...) {

  args <- list(...)

  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$plot.title <- .merge_element(args, object$theme$plot.title)
  object$title <- label

  object
}

#' @rdname add_titel
#' @export
add_caption <- function(object, label, ...) {

  args <- list(...)

  object$caption <- label

  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$plot.caption <- .merge_element(args, object$theme$plot.caption)

  object
}
