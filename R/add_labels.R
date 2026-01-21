#' Add value labels to an scplot
#'
#' @description
#' This function adds value labels to an existing `scplot` object. The labels
#' display the values of a specified variable (default is `.dvar`, which
#' typically represents the main variable being plotted). You can customize the
#' position, rounding, text style, and background style of the labels.
#'
#' @inheritParams .inherit_scplot
#' @param nudge_y Offset on the y-axis.
#' @param nudge_x Offset on the x-axis.
#' @param round Number of digits of the labels. If NULL, no rounding is applied.
#' @param padding Padding size around text.
#' @return An object of class `scplot` (see[scplot()]) with added/changed element
#'   `labels`.
#' @export
add_labels <- function(object,
                       nudge_y = 5,
                       nudge_x = 0,
                       round = NULL,
                       text = list(),
                       background = list(),
                       variable = ".dvar",
                       padding = NULL) {

  if (is.null(padding)) padding <- object$theme$label.padding

  new <- list(
    variable = variable,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    round = round,
    padding = padding,
    text = .merge_element(text, object$theme$label.text),
    background = .merge_element(background, object$theme$label.background)
  )

  object$labels <- c(
    object$label,
    list(new)
  )

  object
}
