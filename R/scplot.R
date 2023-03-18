#' Plot single-case data
#'
#' This function provides a plot of a single-case or multiple single-cases.
#'
#' @param scdf A single-case data-frame object (scdf).
#' @return An object of class `scplot` containing the single-case data (element `scdf`),
#'  and information about the plot style (element `theme`).
#' @author Juergen Wilbert
#' @export

scplot <- function(scdf) {

  scan:::check_args(
    by_class(scdf, "scdf")
  )

  theme <- .scplot_themes[["default"]]

  out <- list(
    scdf = scdf,
    dvar = scdf_attr(scdf, scan:::.opt$dv),
    pvar = scdf_attr(scdf, scan:::.opt$phase),
    mvar = scdf_attr(scdf, scan:::.opt$mt),
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
    casenames = list(labels = revise_names(scdf))
  )

  class(out) <- "scplot"

  out
}

#
revise_names <- function(x, n) {
  names_default <- paste0("Case", 1:50)
  if (missing(n)) {
    n <- length(x)
    if (!is.character(x)) x <- names(x)
  }
  if (is.null(x)) {
    x <- paste0("Case", 1:n)
  } else {
    nonames <- which(is.na(x))
    x[nonames] <- paste0("Case", nonames)
  }

  x
}
