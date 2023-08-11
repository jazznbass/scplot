#' Create a new scplot theme
#'
#' @inheritParams .inherit_scplot
#' @return An scplot-theme object
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
