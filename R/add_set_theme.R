#' Add and set the theme of an scplot
#'
#' @inheritParams .inherit_scplot
#' @param theme A character string with a predefined graphical theme.
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

#' @rdname add_theme
#' @param ... various style parameter
#'
#' @export
set_theme_element <- function(object, ...) {


  object$theme <- .merge_theme(list(...), object$theme)
  object
}
