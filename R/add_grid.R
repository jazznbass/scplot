#' Add grid to an scplot theme
#'
#' Adds grid line specifications to the theme of an `scplot` object.
#'
#' The function allows customization of grid lines by passing arguments
#' similar to those used in `element_line()`, such as `color`, `size`,
#' and `linetype`.
#'
#' @inheritParams .inherit_scplot
#' @param ... Line arguments (see [element_line()]).
#' @return An object of class `scplot` (see[scplot()]) with modified `theme` element
#'  to include the specified grid line properties.
#' @seealso [element_line()]
#' @examples
#' data(exampleAB, package = "scan")
#' p1 <- scplot(exampleAB$Anja)  |>
#'   set_theme("minimal")  |>
#'   add_grid(color = "grey70")
#' @export
add_grid <- function(object, ...) {

  args <- do.call("element_line", list(...))
  object$theme$grid <- args

  object
}
