#' Point element
#' In conjunction with ggplot an object to represent point attributes.
#'
#' @param color Point colour.
#' @param colour Point colour.
#' @param size Relative size.
#' @param shape Point shape.
#' @export
element_point <- function (colour = NULL, size = NULL, shape = NULL,
                           color = NULL, inherit.blank = FALSE)
{
  if (!is.null(color))
    colour <- color
  structure(list(colour = colour, size = size, shape = shape),
            class = c("element_point", "element"))
}

