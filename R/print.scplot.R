#' print.scplot
#'
#' @param x An scplot object
#'
#' @return Creates a single-case plot
#' @export
#' @import scan
#' @import ggplot2

print.scplot <- function(x, ...) {
  args <- list(...)
  p <- as_ggplot(x)
  print(p)
}
