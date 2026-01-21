#' print.scplot - Print an scplot object
#'
#' Prints an `scplot` object by converting it to a ggplot2 object using
#' [as_ggplot()].
#'
#' @param x An scplot object.
#' @param ... Not implemented.
#'
#' @return Calls [as_ggplot()] to create a ggplot2 argument and prints it.
#' @keywords internal
#' @export

print.scplot <- function(x, ...) {
  args <- list(...)
  print(as_ggplot(x))
}
