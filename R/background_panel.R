#' Set plot and panel background of an scplot
#'
#' @inheritParams .inherit_scplot
#' @param ... List with rectangle parameters (`"fill", "colour", "linewidth",
#'   "linetype"`). See [element_rect()].
#' @return An object of class `scplot` (see[scplot()]).
#' @examples
#' p1 <- scplot(exampleAB) %>%
#'  set_background(fill = "lightblue", colour = "darkblue", linewidth = 1.5) %>%
#'  set_panel(fill = "deepskyblue", color = "darkblue", linewidth = 0.3)
#' @export
set_background <- function(object, ...) {

  args <- list(...)
  object$theme$plot.background <- .merge_element(
    args, object$theme$plot.background
  )

  object
}

#' @rdname set_background
#' @export
set_panel <- function(object, ...) {

  args <- list(...)

  if (length(args$fill > 1)) alpha <- 0.5 else alpha <- 1

  if (!is.null(args$fill)) args$fill <- alpha(args$fill, alpha)

  object$theme$panel.background <- .merge_element(
    args, object$theme$panel.background)

  object
}
