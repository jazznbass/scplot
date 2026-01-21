#' Create a new scplot theme object
#'
#' `new_theme()` creates a new theme object for use with scplot.
#'
#' Themes control the overall appearance of scplot visualizations, including
#' colors, fonts, line styles, and layout options. By creating a custom theme,
#' users can ensure consistent styling across multiple plots and tailor the
#' visualizations to their specific needs or branding requirements.
#'
#' @inheritParams .inherit_scplot
#' @examples
#' data(exampleABC, package = "scan")
#' my_theme <- new_theme() |>
#'   set_panel(color = "red")  |>
#'   set_base_text(size = 12, color = "blue")  |>
#'   set_dataline(color = "darkred", linewidth = 2)
#' p1 <- scplot(exampleABC)  |> set_theme(my_theme)
#' @return An object of class `scplot-theme` which can be used with the
#'   [set_theme()] function.
#' @export
new_theme <- function() {

  out <- structure(
    list(
      dvar = ".dvar",
      pvar = ".pvar",
      mvar = ".mvar",
      datalines = list(list(type = "continuous")),
      statlines = NULL,
      ridges = NULL,
      marks = NULL,
      texts = NULL,
      lines = NULL,
      theme = .scplot_themes[["default"]],
      title = NULL,
      caption = NULL,
      xaxis = list(lim = NULL, inc = 1),
      yaxis = list(lim = NULL),
      xlabel = NULL,
      ylabel = NULL,
      labels = list(),
      phasenames = list(labels = ".default"),
      legend = NULL,
      casenames = list(labels = ".default")
    ),
    class = "scplot-theme"
  )
  out
}



#' `extract_theme()` extracts theme from an scplot object
#'
#' `extract_theme()` extracts the theme from an existing scplot object, allowing
#' users to reuse or modify the theme for other scplot visualizations.
#'
#' @export
#' @rdname new_theme
extract_theme <- function(object) {

out <- structure(object,
                 class = "scplot-theme"
)

out$scdf <- NULL
out$dvar <- ".dvar"
out$pvar <- ".pvar"
out$mvar <- ".mvar"
out$casenames <- list(labels = ".default")
out
}
