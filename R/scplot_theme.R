#' Create a new theme for scplot
#'
#' @inheritParams .inherit_scplot
#' @param theme,... A character string with a predefined graphical theme as the basis.
#'
#' @export
scplot_theme <- function(theme, ...) {
  themes <- c(theme, ...)

  if (!(all(themes %in% names(.scplot_themes)))) {
    stop("Unknown theme template.")
  }

  for(i in themes)
   theme <- .merge_theme(.scplot_themes[[i]], theme)

  out <- list(
    theme = theme
  )
  class(out) <- "scplot_theme"
  out
}
