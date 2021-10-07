

#' @rdname scplot
#' @param ... various style parameter
#'
#' @export
set_theme_element <- function(object, ...) {

  object$theme <- utils::modifyList(object$theme, list(...), keep.null = TRUE)
  object
}












#' @rdname scplot
#' @param case Numerical vector with the csae number or character string "all" for all cases.
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the aroow angels.
#' @export
add_arrow <- function(object, case = 1, x0, y0, x1, y1, length = 0.1,
                      color = NULL, ...) {
  arrow <- list(
    case = case,
    x0 = x0,
    y0 = y0,
    x1 = x1,
    y1 = y1,
    length = length,
    col= color,
    ...
  )
  object$arrows <- c(object$arrows, list(arrow))
  object
}



#' @rdname scplot
#' @export
add_ridge <- function(object, color = "grey98") {

  object$theme$ridge.col <- color
  object
}


#' @rdname scplot
#' @export
add_legend <- function(object, labels = ".default", case = 1, x, y,
                       datalines = TRUE, statlines = TRUE, title = NULL) {

  object$legend$labels <- labels
  if (!missing(x)) object$theme$legend.position.x <- x
  if (!missing(y)) object$theme$legend.position.y <- y
  object$legend$statlines <- statlines
  object$legend$datalines <- datalines
  object$theme$legend.position.case <- case
  object$legend$title <- title
  object
}

#' @rdname scplot
#' @param outer Vector with four values for the extension of the outer margins
#' (negative numbers for smaller margins): c(bottom, left, top, right).
#' @param outer Vector with four values for the extension of the inner margins
#' (negative numbers for smaller margins): c(bottom, left, top, right)
#' @export
add_margins <- function(object, outer, inner) {

  if (!missing(outer)) object$theme$oma <- object$theme$oma + outer
  if (!missing(inner)) object$theme$mar <- object$theme$mar + inner


  object
}
