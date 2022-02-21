#' Set and add datalines of an scplot
#'
#' @inheritParams .inherit_scplot
#' @export
add_dataline <- function(object,
                         variable,
                         color,
                         width,
                         linetype,
                         dots,
                         shape,
                         size) {

  object$dvar <- c(object$dvar, variable)

  if (missing(color)) color <- object$theme$dataline.col
  if (missing(width)) width <- object$theme$dataline.width
  if (missing(linetype)) linetype <- object$theme$dataline.linetype
  if (missing(dots)) dots <- color
  if (missing(shape)) shape <- object$theme$datadots.shape
  if (missing(size)) size <- object$theme$datadots.size

  new_line <- list(
    variable = variable,
    col = color,
    width = width,
    linetype = linetype,
    dots = dots,
    shape = shape,
    size = size
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
                         dots,
                         shape,
                         size) {

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
  if (!missing(dots)) object$datalines[[id]]$dots <- dots
  if (!missing(shape)) object$datalines[[id]]$shape <-shape
  if (!missing(size)) object$datalines[[id]]$size <- size

  object
}
