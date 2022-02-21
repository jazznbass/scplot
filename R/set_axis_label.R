#' Set label for axis
#'
#' @inheritParams .inherit_scplot
#'
#' @export
set_xlabel <- function(object, label = NULL, ...) {

  args <- list(...)

  if (!is.null(label)) object$xlabel <- label
  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.title.x <- .merge_element(args, object$theme$axis.title.x)

  object
}

#' @rdname set_xlabel
#' @export
set_ylabel <- function(object, label = NULL, ...) {

  args <- list(...)

  if (!is.null(label)) object$ylabel <- label
  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.title.y <- .merge_element(args, object$theme$axis.title.y)

  object
}



