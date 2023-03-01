#' Set and add datalines of an scplot
#'
#' @inheritParams .inherit_scplot
#' @param type Either "continuous" or "descrete"
#' @param ... As a shortcut, arguments passed hear are bundled as `line`
#'   arguments. E.g. colour = "res", linewidth = 2.
#' @export
add_dataline <- function(object,
                     variable,
                     line,
                     point,
                     type = "continuous",
                     ...) {

  line_args <- list(...)
  if (missing(line)) {
    if (length(line_args) > 0) line <- line_args else line <- list()
  }

  if (missing(point)) point <- list()

  n_lines <- length(object$datalines)
  if (n_lines == length(object$theme$dataline)) n_lines <- 1

  line <- .merge_element(line, object$theme$dataline[[n_lines + 1]])

  if (identical(class(point), "character")) {
    if (!identical(point, "none")) point = list(colour = point)
  } else {
    point <- .merge_element(point, object$theme$datapoint[[n_lines + 1]])
    if (is.null(point$colour)) point$colour <- line$colour
  }

  new_line <- list(variable = variable, type = type)

  object$dvar <- c(object$dvar, variable)
  object$datalines <- c(object$datalines, list(new_line))

  n_element <- length(object$datalines)
  object$theme$dataline[[n_element]] <- line
  object$theme$datapoint[[n_element]] <- point

  object
}
