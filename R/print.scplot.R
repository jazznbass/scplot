#' print.scplot
#'
#' @param x An scplot object.
#' @param ... Not implemented.
#'
#' @return Creates a single-case plot
#' @keywords internal
#' @export

print.scplot <- function(x, ...) {
  args <- list(...)
  print(as_ggplot(x))
}
