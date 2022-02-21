#' Set plot and panel background of an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
set_background <- function(object, ...) {

  args <- list(...)
  object$theme$plot.background <- .merge_element(
    args, object$theme$plot.background)

  object
}

#' @rdname set_background
#' @export
set_panel <- function(object, alpha = NULL, ...) {

  args <- list(...)

  if (is.null(alpha)) {
    if (length(args$fill > 1)) alpha <- 0.5 else alpha <- 1
  }

  if (!is.null(args$fill)) args$fill <- alpha(args$fill, alpha)

  object$theme$panel.background <- .merge_element(
    args, object$theme$panel.background)

  object
}
