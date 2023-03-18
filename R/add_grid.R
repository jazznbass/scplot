#' Add grid to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param ... Line arguments (see [element_line()])
#' @return An object of class `scplot` (see[scplot()]).
#' @seealso [element_line()]
#' @examples
#' p1 <- scplot(exampleAB$Anja)  |>
#'   set_theme("minimal")  |>
#'   add_grid(color = "grey70")
#' @export
add_grid <- function(object, ...) {

  args <- do.call("element_line", list(...))
  object$theme$grid <- args

  object
}
