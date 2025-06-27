#' Add arrrows to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the arrow angels.
#' @param type One of "open" or "closed" indicating whether the arrow head
#'   should be a closed triangle.
#' @param ends One of "last", "first", or "both", indicating which ends of the
#'   line to draw arrow heads.
#' @return An object of class `scplot` (see[scplot()]) with added element
#'   `lines`.
#' @examples
#' data(exampleAB, package = "scan")
#' p1 <- scplot(exampleAB$Anja)  |>
#'   add_arrow(case = 1, 2, 70, 6, 55, color = "darkred")
#' @export
add_arrow <- function(object,
                      case = 1,
                      x0, y0, x1, y1,
                      color = "black",
                      angle = 30,
                      length = unit(5, "points"),
                      type = "open",
                      ends = "last",
                      linewidth = 0.7) {


  line <- list(
    arrow = TRUE,
    case = case,
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    colour = color,
    angle = angle,
    length = length,
    ends = ends,
    type = type,
    linewidth = linewidth
  )

  object$lines <- c(object$lines, list(line))
  object
}
