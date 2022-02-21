#' Dummy function to inherit global descriptions of parameters
#'
#' @param object An scplot object (class `scplot`) returned from the `scplot()`
#'  function.
#' @param label A Character vector with text labels.
#' @param labels A Character vector with text labels.
#' @param color A character string or a number defining the color of an element.
#' @param size Text size relative to the base text size.
#' @param hjust Horizontal justification (in [0,1])
#' @param vjust Vertical justification (in [0,1])
#' @param angle Angle (in [0,360])
#' @param width A number defining the width of the line.
#' @param linetype A character string with the line type: "solid", "dashed",
#'   "dotted"
#' @param shape Number. See pch graphical parameter on par help page.
#' @param text A list with text styling arguments (color, size, face, family,
#'   hjust, vjust, lineheight, angle).
#' @param background A list with background styling arguments (fill, color, size,
#'   linetype).
#' @param case Numerical vector with the case number or character string "all"
#'   for all cases.
#' @param variable Name of the dataline variable to apply the style.
#' @param ... Further styling arguments: color, size, face, family, hjust, vjust, lineheight, angle, linetype, lineend, arrow, fill, margin.
.inherit_scplot <- function(object,label, labels, color, size, hjust, vjust, angle, width, linetype, shape, text, background, case, variable, ...) {}
