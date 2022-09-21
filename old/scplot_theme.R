#' Create a new theme for scplot
#'
#' @param theme,... A character string with a predefined graphical theme as the basis.
#'
#' @export
scplot_theme <- function(theme, ...) {
  themes <- c(theme, ...)

  if (!(all(themes %in% names(.scplot_themes)))) {
    stop(
      "Unknown theme template. ",
      "Available themes are: ",
      paste0("'", names(.scplot_themes), "'", collapse = ", ")
    )
  }

  for(i in themes)
   theme <- .merge_theme(.scplot_themes[[i]], theme)

  out <- list(
    theme = theme
  )
  class(out) <- "scplot_theme"
  out
}
