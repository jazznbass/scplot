#' Plot single-case data
#'
#' This function provides a plot of a single-case or multiple
#' single-cases.
#' @inheritParams .inheritParams
#' @param data A single-case data-frame object (scdf).
#' @return An scplot object that creates a ggplot2 plot when printed.
#' @author Juergen Wilbert
#' @export

scplot <- function(data) {

  data <- scan:::.prepare_scdf(data)

  theme <- .scplot_themes[["default"]]

  default_line <- list(
    variable = NULL,
    col = NULL,
    width = NULL,
    linetype = NULL,
    dots = NULL,
    shape = NULL,
    size = NULL
  )

  out <- list(
    scdf = data,
    dvar = scdf_attr(data, scan:::.opt$dv),
    pvar = scdf_attr(data, scan:::.opt$phase),
    mvar = scdf_attr(data, scan:::.opt$mt),
    datalines = list(default_line),
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
    casenames = list(labels = scan:::.case_names(names(data), length(data)))
  )

  class(out) <- "scplot"

  out
}

