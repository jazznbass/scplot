#' @rdname scplot
#' @param theme A character string with a predefined graphical theme.
#' Possible values: default, yaxis, tiny, small, big, chart, ridge,
#' annotate, grid, grid2, dark, nodots, sienna, phase_color, phase_shade
#'
#' @export
add_theme <- function(object, theme, ...) {

  themes <- c(theme, ...)

  if (!(all(themes %in% names(.scplot_themes)))) {
    stop("Unknown theme template.")
  }

  for(i in themes)
    object$theme <- .merge_theme(.scplot_themes[[i]], object$theme)

  object

}

#' @rdname scplot
#' @param ... various style parameter
#'
#' @export
set_theme_element <- function(object, ...) {

  object$theme <- utils::modifyList(object$theme, list(...), keep.null = TRUE)
  object
}







#' @rdname scplot
#' @param positions Either a vector indicating the dot to be highlighted or a
#' character string with a logical expression (e.g. values < mean(values))
#' @param shape Number. See pch graphical parameter on par help page.
#' @export
add_marks <- function(object, case = 1, positions,
                      color = "red", size = 1, shape = 1, variable = ".dvar") {

  # Marks on the outliers from outlier()
  if (identical(class(positions), "sc_outlier")) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks,
        list(
          list(
            case = i, positions = positions$dropped.mt[[i]],
            col = color, cex = size, pch = shape, variable = variable
          )
        )
      )
    return(object)
  }

  for(i in case) {
    object$marks <- c(
      object$marks,
      list(
        list(
          case = i, positions = positions, col = color,
          cex = size, pch = shape, variable = variable
        )
      )
    )
  }


  object
}


#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object, case = 1, x, y, label, color = NULL, size = NULL,
                     angle = 0) {

  text <- list(case = case, labels = label, x = x, y = y, col = color, cex = size, angle = angle)
  object$texts <- c(object$texts, list(text))
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
