#' Add a statline to an scplot object
#'
#' This function adds a statistical line or curve to an existing `scplot`
#' object. Various statistical functions are available such as mean, median,
#' min, max, quantile, standard deviation, moving average, and trend lines.
#'
#' @inheritParams .inherit_scplot
#' @param stat A character string for defining a statistical line or curve to be
#'   plotted. Main options are `"mean"`, `"median"`, `"min"`, `"max"`,
#'   `"quantile"`, `"sd"`, `"mad"`, `"trend"`, `"moving mean"`, `"moving
#'   median"`, `"loreg"` (local regression using `loess`).
#' @param phase Either a numeric or a character vector specifying the reference
#'   phase (see details).
#' @param label A character string which is used to set the label in a legend.
#' @param ... additional parameters passed to the statistical function.
#' @param segmented Logical. If TRUE, the statline is plotted separately for
#'   each phase. The default is NULL, where sensible settings are applied based
#'   on the `stat` and `phase` arguments.
#' @param case Either a numeric or a character vector specifying the case(s) for which the
#'   statline is plotted. The default is NULL, which applies the statline to all
#'   cases.
#' @details The `phase` argument defines the reference phase for some
#'   statistical functions (`"median", "mean", "min", "max", "quantile",
#'   "trend"`). The default is `NULL` which calculates and plots statistics for
#'   each phase separately. The arguments takes a numeric vector (phase
#'   number(s)) or a character vector (phase name(s)). When more than one phase
#'   is defines, statistics are based on the combined values of these phases.
#'   `phase = all` will select all phases. Various methods for a *trend* line
#'   exist that can be set with `method` argument: The default is based on an
#'   OLS regression, `"theil-sen"` on a nonparametric regression, and
#'   `"bisplit"` / `"trisplit"` are two median based approaches. Some of the
#'   functions defined in `stat` have additional arguments. The [mean()]
#'   function has a trim argument (e.g. `trim = 0.1`). [quantile()] has a
#'   proportion argument (e.g. `prob = 0.75` for calculating the 75% quantile).
#'   `moving mean` and `moving median` have a lag argument (e.g. `lag = 2`). The
#'   local-regression curve function `"lowess"` (or `"loreg"`) has a proportion
#'   argument (e.g. `f = 0.5`; see [lowess()]) and the local-regression curve
#'   function `"loess"` has a span argument (e.g. `span = 0.75`; see [loess()]).
#' @return An object of class `scplot` (see[scplot()]) with changed element
#'   `statlines`.
#' @export
add_statline <- function(object,
                         stat = c("mean", "median", "min", "max", "quantile",
                                  "sd", "mad", "trend",
                                  "moving mean", "moving median",
                                  "loreg", "lowess", "loess",
                                  "trendA", "trendA theil-sen",
                                  "trendA bisplit", "trendA trisplit"),
                         phase = NULL,
                         color = NULL,
                         linewidth = NULL,
                         linetype = NULL,
                         variable = NULL,
                         label = NULL,
                         segmented = NULL,
                         case = NULL,
                         ...) {

  stat <- match.arg(stat)

  if (is.null(variable)) variable <- ".dvar"

  # add line element for statline ----
  line <- list(
    linewidth = linewidth,
    colour = color,
    linetype = linetype
  )

  n_lines <- length(object$statlines)
  if (n_lines == length(object$theme$statline)) n_lines <- 1
  line <- .merge_element(
    line,
    object$theme$statline[[n_lines + 1]]
  )
  object$theme$statline[[length(object$statlines) + 1]] <- line

  args <- list(...)

  # organize arguments for trend stat ----

  if (stat == "trend" && is.null(args$method)) {
    args$method <- "lm"
  }

  if (stat == "trendA theil-sen") {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "theil-sen"
  }

  if (stat == "trendA bisplit") {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "bisplit"
  }

  if (stat == "trendA trisplit") {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "trisplit"
  }

  if (stat == "trendA" && (is.null(args$method) || args$method == "lm")) {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "lm"
  }

  if (stat == "trendA" && args$method == "bisplit") {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "bisplit"
  }

  if (stat == "trendA" && args$method == "trisplit") {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "trisplit"
  }

  if (stat == "trendA" && args$method == "theil-sen") {
    if (is.null(phase)) phase <- 1
    stat <- "trend"
    args$method <- "theil-sen"
  }

  # set default label ----
  if (is.null(label)) {
    if (is.null(phase)) phase_str <- NULL
    if (!is.null(phase)) {
      if (is.numeric(phase)) {
        if (length(phase) == 1) {
          if (identical(phase, 1)) {
            phase_str <- paste0(" baseline")
          } else {
            phase_str <- paste0(" phase ", phase)
          }
        } else {
          phase_str <- paste0(" phases ", paste0(phase, collapse = "/"))
        }
      } else {
        phase_str <- paste0(" ", paste0(phase, collapse = "/"))
      }
    }

    stat_str <- stat
    if (stat == "trend" && args$method == "theil-sen") stat_str <- "trend theil-sen"
    if (stat == "trend" && args$method == "bisplit") stat_str <- "trend bisplit"
    if (stat == "trend" && args$method == "trisplit") stat_str <- "trend trisplit"
    label <- paste0(stat_str, " ", variable, phase_str)
  }

  # set default segmented option for statlines ----
  if (is.null(segmented)) {
    segmented <- FALSE
    stat_selection <- c("mean", "median", "min", "max", "quantile",
                        "sd", "mad", "trend")
    if (stat %in% stat_selection && is.null(phase)) {
      segmented <- TRUE
    }
  }

  # add statline to object ----
  new <- list(
    stat = stat,
    phase = phase,
    args = args,
    variable = variable,
    label = label,
    segmented = segmented,
    case = case
  )

  object$statlines <- c(object$statlines, list(new))
  object

}
