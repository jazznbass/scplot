#' Plot single-case data
#'
#' This function provides a plot of a single-case or multiple single-cases.
#'
#' @param scdf A single-case data-frame object (scdf).
#' @return An scplot object that creates a ggplot2 plot when printed.
#' @author Juergen Wilbert
#' @export

scplot <- function(scdf) {

  theme <- .scplot_themes[["default"]]

  out <- list(
    scdf = scdf,
    dvar = scan:::dv(scdf),
    pvar = scan:::phase(scdf),
    mvar = scan:::mt(scdf),
    datalines = list(list(type = "continuous")),
    statlines = NULL,
    ridges = NULL,
    marks = NULL,
    texts = NULL,
    arrows = NULL,
    theme = theme,
    title = NULL,
    caption = NULL,
    xaxis = list(lim = NULL, inc = 1),
    yaxis = list(lim = NULL),
    xlabel = NULL,
    ylabel = NULL,
    labels = list(),
    phasenames = list(labels = ".default"),
    legend = NULL,
    casenames = list(labels = scan:::revise_names(scdf))
  )

  class(out) <- "scplot"

  out
}

