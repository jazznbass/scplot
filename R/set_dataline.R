#' Set data lines of an scplot object
#'
#' Either set aesthetics of the default data line or add another data line.
#'
#' The function allows customization of data lines by passing arguments such as
#' `color`, `size`, and `linetype`. If `variable` is left empty or set to
#' `".dvar"`, the aesthetics of the default data line are changed. Otherwise, a
#' new data line is added for the specified variable.
#'
#' @inheritParams .inherit_scplot
#' @param variable Character with the name of a new variable for adding a new
#'   line. If left empty, the aesthetics of the default data line are changed.
#' @param type Either "continuous" or "discrete". Specifies how the data line
#'   should be treated.
#' @param label A character string which is used to set the label in a legend.
#' @param show_gaps Logical. If TRUE, missing values in the data will result in
#'   gaps in the line. If FALSE, missing values will be ignored and the line
#'   will be drawn continuously.
#' @param ... As a shortcut, arguments passed here are bundled as `line`
#'   arguments (see [element_line()]).
#' @return An object of class `scplot` (see[scplot()]) with a changed
#'   `datalines` element.
#' @seealso [element_line()], [element_point()]
#' @examples
#' data(exampleAB_add, package = "scan")
#' scplot(exampleAB_add)  |>
#'   set_dataline("depression", color = "darkblue") |>
#'   set_dataline(color = "darkgreen", point = list(shape = 5, size = 3))
#' @export
set_dataline <- function(object,
                         variable = NULL,
                         line,
                         point,
                         type = "continuous",
                         label = NULL,
                         show_gaps = FALSE,
                         ...) {

  line_args <- list(...)

  if (missing(line)) {
    if (length(line_args) > 0) line <- line_args else line <- list()
  }
  if (missing(point)) point <- list()
  if (is.null(variable)) variable <- ".dvar"

  if (identical(variable, ".dvar")) {
    pos_line <- 1
    n_element <- 1
    object$datalines[[1]]$type <- type
    object$datalines[[1]]$show_gaps <- show_gaps
    if (!is.null(label)) object$datalines[[1]]$label <- label
  } else {
    pos_line <- length(object$datalines) + 1
    if (pos_line > length(object$theme$dataline)) pos_line <- 1
    n_element <- length(object$datalines) + 1
    object$dvar <- c(object$dvar, variable)
  }

  if (inherits(point, "character")) point <- element_point(colour = point)

  if (!inherits(point, "ggplot2::element_point")) {
    point <- do.call(element_point, point)
  }
  if (!inherits(line, "ggplot2::element_line")) {
    line <- do.call(element_line, line)
  }

  line <- .merge_element(line, object$theme$dataline[[pos_line]])
  point <- .merge_element(point, object$theme$datapoint[[pos_line]])

  if (is.null(point@colour)) point@colour <- line@colour

  new_line <- list(
    variable = variable,
    type = type,
    label = label,
    show_gaps = show_gaps
  )

  object$datalines[[n_element]] <- new_line
  object$theme$dataline[[n_element]] <- line
  object$theme$datapoint[[n_element]] <- point

  object
}

#' @rdname set_dataline
#' @export
add_dataline <- function(...) {
  warn("Deprecated. Use `set_dataline()` instead.")
  set_dataline(...)
}
