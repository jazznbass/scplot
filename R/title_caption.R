#' Add title and caption to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param ... List with text parameters (`"family", "face", "colour", "size",
#'   "hjust", "vjust", "angle", "lineheight", "margin"`). See [element_text()].
#' @return An object of class `scplot` (see[scplot()]) with changed `title`
#'   and `caption` elements.
#' @export
add_title <- function(object, label,...) {

  args <- list(...)

  if (!is.null(args$size)) args$size <- rel(args$size)

  if (missing(label)) label <- ""

  object$theme$plot.title <- .merge_element(args, object$theme$plot.title)
  object$title <- label

  object
}

#' @rdname add_title
#' @param header String with header above footnote/ caption
#' @export
add_caption <- function(object, label, header = "Note:\n", ...) {

  args <- list(...)
  if (missing(label)) label <- ""
  object$caption <- paste0(header, paste0(label, collapse = "\n"))

  if (!is.null(args$size)) args$size <- rel(args$size)

  object$theme$plot.caption <- .merge_element(args, object$theme$plot.caption)

  object
}
