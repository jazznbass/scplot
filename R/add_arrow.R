#' Add an arrow to a scplot
#'
#' Draws an arrow between two points in the scplot.
#'
#' @details The arrow is drawn from point `(x0, y0)` to point `(x1, y1)`. The
#'   arrow head can be customized with the arguments `angle`, `length`, `type`,
#'   and `ends`. The `angle` argument specifies the angle of the arrow head,
#'   while `length` specifies the size of the arrow head. The `type` argument
#'   indicates whether the arrow head should be an open or closed triangle, and
#'   the `ends` argument specifies which ends of the line should have arrow
#'   heads (last, first, or both). The `color` and `linewidth` arguments control
#'   the appearance of the arrow line. Note that the `length` argument can be
#'   specified as a numeric value (in points) or as a unit object (e.g.,
#'   `unit(5, "points")`).
#'
#' @inheritParams .inherit_scplot
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the arrow angels. Can be specified as a numeric value
#'   (in points) or as a unit object (e.g., `unit(5, "points")`).
#' @param type One of `"open"` or `"closed"` indicating whether the arrow head
#'   should be a closed triangle.
#' @param ends One of `"last"`, `"first"`, or `"both"`, indicating which ends of
#'   the line to draw arrow heads.
#' @return An object of class `scplot` (see [scplot()]) with added element
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
                      length = 5,
                      type = "open",
                      ends = "last",
                      linewidth = 0.7) {

 if (inherits(length, "numeric")) length <- unit(length, "points")

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
