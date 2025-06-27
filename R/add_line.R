#' Add line to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param hline y position of horizontal line.
#' @param vline x position of vertical line.
#' @return An object of class `scplot` (see[scplot()]) with added element
#'   `lines`.
#' @examples
#' data(exampleAB, package = "scan")
#' p1 <- scplot(exampleAB$Anja)  |>
#'   add_line(hline = 70, color = "darkred") |>
#'   add_line(vline = 3, color = "blue") |>
#'   add_line(x0 = 1, y0 = 70, x1 = 4, y1 = 80, color = "green")
#' @export
add_line <- function(object,
                     case = 1,
                     x0 = NULL,
                     y0 = NULL,
                     x1 = NULL,
                     y1 = NULL,
                     hline = NULL,
                     vline = NULL,
                     color = "black",
                     linewidth = 0.7,
                     linetype = "solid") {

  if(!is.null(hline) && !is.null(vline)) {
    stop("Either choose a vline or an hline.")
  }

  line <- list(
    arrow = FALSE,
    case = case,
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    colour = color,
    linewidth = linewidth,
    linetype = linetype,
    hline = hline,
    vline = vline
  )
  object$lines <- c(object$lines, list(line))
  object
}
