#' Add a statline to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param stat A character string for defining a statistical line or curve to
#'   be plotted. Possible values: "median", "mean", "min", "max", "quantile",
#'   "trend", "trendA",
#'   "trendA_bisplit", "trendA_trisplit",
#'   "movingMean", "movingMedian", "loreg".
#' @param phase Either a numeric or a character vector specifying the reference
#'   phase (see details)
#' @param ... additional parameters passed to the statistical function.
#' @details The 'phase' argument defines the reference phase for some
#'   statistiscal functions ("median", "mean", "min", "max", "quantile").
#'   The default is NULL which calculates and plots statistics for each phase
#'   separately. The arguments takes a numeric vector (phase number(s)) or a
#'   character vector (phase name(s)). When more than one phase is defines,
#'   statistics are based on the combined values of these phases.
#'   Some of the functions defined in 'stats' have additional arguments.
#'   The 'mean' function has a trim argument (e.g. 'trim = 0.1').
#'   'quantile has a proportion argument (e.g. prob = 0.75 for calculating
#'   the 75% quantile).
#'   'movingMean' and 'movingMedian' have a lag argument (e.g. lag = 2).
#'   The local-regression curve function 'lowess' (or 'loreg') has a proportion
#'   argument (e.g. f = 0.5) and the
#'   local-regression curve function 'loess' has a span argument
#'   (e.g. span = 0.75).
#'
#' @export
add_statline <- function(object,
                         stat,
                         phase = NULL,
                         color = NULL,
                         width = NULL,
                         linetype = NULL,
                         variable = NULL,
                         ...) {

  if (is.null(color)) color <- object$theme$statline$colour
  if (is.null(width)) width <- object$theme$statline$size
  if (is.null(linetype)) linetype <- object$theme$statline$linetype
  if (is.null(variable)) variable <- ".dvar"

  new <- list(
    stat = stat,
    phase = phase,
    args = list(...),
    line = element_line(
      colour = color,
      size = width,
      linetype = linetype
    ),
    variable = variable
  )

  object$statlines <- c(object$statlines, list(new))
  object

}
