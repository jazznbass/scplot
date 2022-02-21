#' Add arrrows to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the arrow angels.
#' @param type One of "open" or "closed" indicating whether the arrow head should be a closed triangle.
#' @export
add_arrow <- function(object,
                      case = 1,
                      x0, y0, x1, y1,
                      color = "black",
                      angle = 30,
                      length = unit(5, "points"),
                      type = "open") {
  arrow <- list(
    case = case,
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    colour = color,
    angle = angle,
    length = length,
    ends = "last",
    type = type
  )
  object$arrows <- c(object$arrows, list(arrow))
  object
}
