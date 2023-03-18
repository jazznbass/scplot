#' Set data lines of an scplot
#'
#' Either set aesthetics of the default data line or add another data line.
#'
#' @inheritParams .inherit_scplot
#' @param variable String. The name of a new variable for adding a new line. If
#'   left empty, the aesthetics of the default data line are changed.
#' @param type Either "continuous" or "discrete"
#' @param ... As a shortcut, arguments passed hear are bundled as `line`
#'   arguments (see [element_line()]).
#' @return An object of class `scplot` (see[scplot()]) with a changed `datalines`
#'   element.
#' @seealso [element_line()], [element_point()]
#' @examples
#' p1 <- scplot(exampleAB_add) %>%
#'   set_dataline("depression", color = "darkblue")
#' @export
set_dataline <- function(object,
                         variable = NULL,
                         line,
                         point,
                         type = "continuous",
                         ...) {

  line_args <- list(...)
  if (missing(line)) {
    if (length(line_args) > 0) line <- line_args else line <- list()
  }

  if (missing(point)) point <- list()

  if (identical(variable, ".dvar") || is.null(variable)) {
    return(.set_dataline(object, variable, line, point, type))
  }

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

.set_dataline <- function(object,
                          variable,
                          line,
                          point,
                          type) {

  id <- 1
  object$datalines[[id]]$type <- type

  object$theme$dataline[[id]] <- .merge_element(
    line, object$theme$dataline[[id]]
  )

  if (!identical(point, "none")) {
    point <- .merge_element(point, object$theme$datapoint[[id]])
  }

  object$theme$datapoint[[id]] <- point

  object
}

#' @rdname set_dataline
#' @export
add_dataline <- function(...) {
  warning("Deprecated. Use `set_dataline()` instead.")
  set_dataline(...)
}
