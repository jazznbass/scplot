

#' @rdname scplot
#' @export
add_legend <- function(object, labels = ".default", case = 1, x, y,
                       datalines = TRUE, statlines = TRUE, title = NULL) {

  object$legend$labels <- labels
  if (!missing(x)) object$theme$legend.position.x <- x
  if (!missing(y)) object$theme$legend.position.y <- y
  object$legend$statlines <- statlines
  object$legend$datalines <- datalines
  object$theme$legend.position.case <- case
  object$legend$title <- title
  object
}

