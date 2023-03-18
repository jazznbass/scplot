#' Add test to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param x x position
#' @param y y position
#' @return An object of class `scplot` (see[scplot()]) with a changed `texts`
#'   element.
#' @export
add_text <- function(object,
                     label,
                     case = 1,
                     x,
                     y,
                     color = "black",
                     size = 1,
                     angle = 0,
                     hjust = 0.5,
                     vjust = 0.5,
                     face = 1) {

  text <- list(
    case = case,
    labels = label,
    x = x,
    y = y,
    colour = color,
    size = size,
    angle = angle,
    hjust = hjust,
    vjust = vjust,
    face = face
  )

  object$texts <- c(object$texts, list(text))
  object
}
