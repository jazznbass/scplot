#' Set and add datalines of an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
add_dataline <- function(object,
                         variable,
                         color,
                         width,
                         linetype,
                         point,
                         type = "continuous") {

  object$dvar <- c(object$dvar, variable)

  if (missing(color)) color <- object$theme$dataline.col
  if (missing(width)) width <- object$theme$dataline.width
  if (missing(linetype)) linetype <- object$theme$dataline.linetype

  if (missing(point)) point <- list()
  if (identical(class(point), "character")) {
    if (!identical(point, "none")) point = list(colour = point)
  }
  point <- .merge_element(point, object$theme$datapoint)
  if (is.null(point$color)) point$color <- color

  new_line <- list(
    variable = variable,
    col = color,
    width = width,
    linetype = linetype,
    point = point,
    type = type
  )

  object$datalines <- c(object$datalines, list(new_line))

  object
}

#' @rdname add_dataline
#' @export
set_dataline <- function(object,
                         variable,
                         color,
                         width,
                         linetype,
                         point,
                         type) {

  if (missing(variable)) variable <- ".dvar"

  vars <- sapply(
    object$datalines,
    function(x) if(is.null(x$variable)) ".dvar" else x$variable
  )
  id <- which(vars == variable)

  if (length(id) != 1) stop("Wrong variable defintion.")

  if (!missing(color)) object$datalines[[id]]$col <- color
  if (!missing(width)) object$datalines[[id]]$width <- width
  if (!missing(linetype)) object$datalines[[id]]$linetype <- linetype
  if (!missing(type)) object$datalines[[id]]$type <- type

  if (missing(point)) point <- list()

  if (identical(class(point), "character")) {
    if (!identical(point, "none")) point = list(colour = point)
  }

  point <- .merge_element(point, object$theme$datapoint)

  object$datalines[[id]]$point <- point



  object
}

#' @rdname add_dataline
#' @export
# add_data <- function(object,
#                      variable,
#                      line,
#                      color,
#                      width,
#                      linetype,
#                      point,
#                      type = "continuous") {
#
#   if (missing(point)) point <- list()
#   if (missing(line)) line <- list()
#
#   line <- .merge_element(line, object$theme$dataline)
#
#   if (identical(class(point), "character")) {
#     if (!identical(point, "none")) point = list(colour = point)
#   }
#   point <- .merge_element(point, object$theme$datapoint)
#   if (is.null(point$color)) point$color <- color
#
#
#
#   new_line <- list(
#     variable = variable,
#     col = color,
#     width = width,
#     linetype = linetype,
#     line = line,
#     point = point,
#     type = type
#   )
#
#   object$dvar <- c(object$dvar, variable)
#   object$datalines <- c(object$datalines, list(new_line))
#
#   object
# }
