#' Set a theme element
#'
#' @inheritParams .inherit_scplot
#' @param ... various style parameter
#' @details Usually, you don't need this function. Possible theme elements are.
#' "text", "plot.background", "panel.background", "panel.spacing.y",
#' "dataline", "datapoint", "statline", "axis.line.x", "axis.line.y",
#' "axis.ticks.length", "axis.ticks", "axis.title.y", "axis.title.x",
#' "axis.text.x", "axis.text.y", "plot.title", "plot.caption", "plot.margin",
#' "casenames", "casenames.strip", "casenames.position", "phasenames",
#' "phasenames.position.x", "separators", "separators.extent", "label.text",
#' "label.background", "label.padding", "grid", "legend.position",
#' "legend.background", "legend.text", "legend.title", "legend.margin"
#' @export
set_theme_element <- function(object, ...) {

  object$theme <- .merge_theme(list(...), object$theme)
  object
}
