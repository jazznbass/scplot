#' Point element for ggplot themes
#'
#' In conjunction with ggplot an object to represent point attributes. Can be
#' used in themes to set default point attributes for geoms that use points.
#' See [ggplot2::element_point()].
#'
#' @param color,colour Point colour.
#' @param size Relative size.
#' @param shape Point shape.
#' @return An object of class `c("element_point", "element")`.
#' @export
element_point <- function (colour = NULL,
                           size = NULL,
                           shape = NULL,
                           color = NULL){
  if (!is.null(color)) colour <- color
  structure(
    list(colour = colour, size = size, shape = shape),
    class = c("element_point", "element")
  )
}

