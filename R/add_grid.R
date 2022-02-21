#' Add grid to an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
add_grid <- function(object, ...) {

  args <- do.call("element_line", list(...))
  object$theme$grid <- args

  object
}
