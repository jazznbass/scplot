#' Plot single-case data
#'
#' This function provides a plot of a single-case or multiple
#' single-cases.
#'
#' @return An scplot object that creates a ggplot2 plot when printed.
#' @author Juergen Wilbert
#' @export
#' @import scan
#' @import ggplot2
#' @importFrom dplyr summarise full_join group_by filter

scplot <- function(data) {

  data <- scan:::.prepare_scdf(data)

  theme <- .merge_theme(.scplot_themes[["grid"]], .scplot_themes[["default"]])

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
    ridges = NULL,
    statlines = NULL,
    theme = theme,
    title = NULL,
    caption = NULL,
    xaxis = list(lim = NULL, inc = 1),
    yaxis = list(lim = NULL),
    xlabel = NULL,
    ylabel = NULL,
    labels = NULL,
    marks = NULL,
    texts = NULL,
    arrows = NULL,
    phasenames = list(labels = ".default"),
    legend = NULL,
    casenames = list(labels = scan:::.case_names(names(data), length(data)))
  )

  class(out) <- "scplot"

  out
}

