#' Plot single-case data
#'
#' This function provides a plot of a single-case or multiple single-cases.
#'
#' @aliases scplot scplot.scdf
#' @param object A single-case data-frame object (scdf).
#' @param ... further arguments.
#' @return An object of class `scplot` containing the single-case data (element `scdf`),
#'  and information about the plot style (element `theme`).
#' @author Juergen Wilbert
#' @export
scplot.scdf <- function(object, ...) {

  caption <- getOption("scplot.plot.caption")
  if (caption == "auto")
    caption <- format_caption(
      scdf_attr(object)$info,
      scdf_attr(object)$author
    )

  out <- list(
    scdf = object,
    dvar = scdf_attr(object)$var.values,
    pvar = scdf_attr(object)$var.phase,
    mvar = scdf_attr(object)$var.mt,
    datalines = list(list(type = "continuous")),
    statlines = NULL,
    ridges = NULL,
    marks = NULL,
    texts = NULL,
    lines = NULL,
    theme = .scplot_themes[[getOption("scplot.plot.theme")]],
    title = NULL,
    caption = caption,
    xaxis = list(lim = NULL, inc = 1),
    yaxis = list(lim = NULL),
    xlabel = NULL,
    ylabel = NULL,
    labels = list(),
    phasenames = list(labels = ".default"),
    legend = NULL,
    casenames = list(labels = revise_names(object))
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

format_caption <- function(info, author, width = 100) {
  caption <- NULL

  if (!is.null(info)) caption <- info

  if (!is.null(author)) {
    if (is.null(caption)) {
      caption <- paste0("Source: ", author)
      caption <- paste(strwrap(caption, width = width), collapse = "\n")
    } else {
      caption <- paste0(
        caption,
        "\nSource: ",
        author
      )
    }

  }

  if (!is.null(caption))  {
    caption <- paste0(
      "Note.\n",
      paste0(strwrap(caption, width = 100), collapse = "\n")
    )
  }

  caption
}


