#' Add a theme of to an scplot
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

