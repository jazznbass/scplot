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
  p <- as_ggplot(x)
  sc <- attr(p, "scplot_staircase")
  if (!is.null(sc)) {
    #print(p)
    x_pos <- split(sc$data$x, factor(sc$data$case, levels = unique(sc$data$case))) |> as.data.frame()
    x_pos <- apply(x_pos, 1, function(x) x,simplify = FALSE)
    suppressMessages(add_staircase(p,
                  x_pos = x_pos,
                  color = sc$color,
                  linewidth = sc$linewidth,
                  linetype = sc$linetype))
    #print(g)
  } else {
    print(p)
  }
}
