#' Set axis parameters of an scplot
#'
#' Sets limits, increments, line and text parameters of x- or y-axis of an
#' `scplot` object.
#'
#' @inheritParams .inherit_scplot
#' @param limits Lower and upper limits of the axis (e.g., `limits = c(0, 20)`
#'   sets the axis to a scale from 0 to 20). With multiple single-cases you can
#'   use `limits = c(0, NA)` to scale the axis from 0 to the maximum of each
#'   case. `limits` is not set by default, which makes `scplot` set a proper
#'   scale based on the given data.
#' @param expand Vector with two values. Expansion of the axis. First value expands
#'  the lower end of the axis, second value the upper end. Default is `c(0,0)`,
#'  meaning no expansion. You can also use relative values, e.g. `c(0.05, 0.05)`
#'  to expand both ends by 5 percent of the total axis length.
#'  See [ggplot2::scale_x_continuous()] for details.
#' @param increment An integer. Increment of the x-axis. 1 :each mt value will
#'   be printed, 2 : every other value, 3 : every third values etc. If NULL,
#'   `scplot` chooses an appropriate increment based on the data.
#' @param increment_from Number from which increment starts to count. Usually
#'   set to 0 if you want marks like 1,5,10,15,... on the axis. If NULL, `scplot`
#'   chooses an appropriate value based on the data.
#' @return An object of class `scplot` (see[scplot()]) with changed `xaxis` and
#'   `yaxis` elements. Also, the `theme` element is changed to reflect line and text
#'   parameters.
#' @export
set_xaxis <- function(object,
                      limits = NULL,
                      increment = NULL,
                      increment_from = NULL,
                      line = NULL,
                      expand = NULL,
                      ...) {


  args <- list(...)

  if (!is.null(limits)) object$xaxis$lim <- limits
  if (!is.null(increment)) object$xaxis$inc <- increment
  if (!is.null(increment_from)) object$xaxis$inc_from <- increment_from
  if (!is.null(expand)) object$theme$axis.expand.x <- expand
  if (!is.null(args$size)) args$size <- rel(args$size)

  if (!is.null(line)) object$theme$axis.line.x <- .merge_element(
    line, object$theme$axis.line.x)
  object$theme$axis.text.x <- .merge_element(args, object$theme$axis.text.x)

  object
}

#' @rdname set_xaxis
#' @export
set_yaxis <- function(object,
                      limits = NULL,
                      increment = NULL,
                      increment_from = NULL,
                      line = NULL,
                      expand = NULL,
                      ...) {

  args <- list(...)

  if (!is.null(limits)) object$yaxis$lim <- limits
  if (!is.null(increment)) object$yaxis$inc <- increment
  if (!is.null(increment_from)) object$yaxis$inc_from <- increment_from
  if (!is.null(expand)) object$theme$axis.expand.y <- expand
  if (!is.null(args$size)) args$size <- rel(args$size)

  if (!is.null(line)) object$theme$axis.line.y <- .merge_element(
    line, object$theme$axis.line.y)
  object$theme$axis.text.y <- .merge_element(args, object$theme$axis.text.y)

  object
}

#' Set label for axis
#'
#' @inheritParams .inherit_scplot
#' @return An object of class `scplot` (see[scplot()]) with a changed `xlabel`
#'   or `ylabel` element.
#' @export
set_xlabel <- function(object, label = NULL, ...) {

  args <- list(...)

  if (!is.null(label)) object$xlabel <- label
  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.title.x <- .merge_element(args, object$theme$axis.title.x)

  object
}

#' @rdname set_xlabel
#' @export
set_ylabel <- function(object, label = NULL, ...) {

  args <- list(...)

  if (!is.null(label)) object$ylabel <- label
  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$axis.title.y <- .merge_element(args, object$theme$axis.title.y)

  object
}
