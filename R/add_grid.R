#' Add grid to an scplot
#'
#' @inheritParams .inherit_scplot
#' @example
#' p1 <- scplot(exampleAB$Anja) %>%
#'   add_theme("minimal") %>%
#'   add_grid(color = "grey70")
#' @export
add_grid <- function(object, ...) {

  args <- do.call("element_line", list(...))
  object$theme$grid <- args

  object
}
