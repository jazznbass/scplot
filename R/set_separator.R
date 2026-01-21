#' Set separator line in an scplot object
#'
#' This function allows you to customize the appearance of separator lines in an
#' `scplot` object by specifying parameters such as color, line width, and line
#' type. You can pass these parameters as arguments, similar to those used in
#' `element_line()`, to modify the visual style of the separator lines in your plot
#' theme.
#'
#' @inheritParams .inherit_scplot
#' @param ... List with line parameters (`"colour"", "linewidth", "linetype"`).
#'   See [element_line()].
#' @return An object of class `scplot` (see[scplot()]) with modified `theme` element.
#' @export
set_separator <- function(object, ...) {

  args <- list(...)
  object$theme$separators <- .merge_element(
    args, object$theme$separators
  )

  object
}
