#' @rdname add_dataline
#' @export
set_dataline <- function(object,
                         variable,
                         line,
                         point,
                         type,
                         ...) {

  if (missing(variable)) variable <- ".dvar"

  vars <- sapply(
    object$datalines,
    function(x) if(is.null(x$variable)) ".dvar" else x$variable
  )
  id <- which(vars == variable)

  if (length(id) != 1) stop("Wrong variable defintion.")

  line_args <- list(...)
  if (missing(line)) {
    if (length(line_args) > 0) line <- line_args else line <- list()
  }

  if (missing(point)) point <- list()

  if (!missing(type)) object$datalines[[id]]$type <- type

  object$theme$dataline[[id]] <- .merge_element(
    line, object$theme$dataline[[id]]
  )

  if (!identical(point, "none")) {
    point <- .merge_element(point, object$theme$datapoint[[id]])
  }

  object$theme$datapoint[[id]] <- point

  object
}
