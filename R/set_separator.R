#' Set separator line in an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
set_separator <- function(object, ...) {

  args <- list(...)
  object$theme$separators <- .merge_element(
    args, object$theme$separators
  )

  object
}
