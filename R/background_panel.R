#' Set plot and panel background of an scplot object
#'
#' The `set_background()` function allows you to customize the plot background
#' of an `scplot` object by specifying parameters such as fill color, border color,
#' line width, and line type. Similarly, the `set_panel()` function enables you
#' to set the panel background with the same customizable parameters. These
#' functions enhance the visual appearance of your plots by allowing you to
#' tailor the background styles to your preferences.
#'
#' @inheritParams .inherit_scplot
#' @param ... List with rectangle parameters (`"fill", "colour", "linewidth",
#'   "linetype"`). See [element_rect()].
#' @return An object of class `scplot` (see[scplot()]).
#' @examples
#' data(exampleAB, package = "scan")
#' p1 <- scplot(exampleAB)  |>
#'  set_background(fill = "lightblue", colour = "darkblue", linewidth = 1.5) |>
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
  if (length(args)== 0) return(object)

  if (length(args$fill > 1)) alpha <- 0.5 else alpha <- 1

  if (!is.null(args$fill)) args$fill <- alpha(args$fill, alpha)

  object$theme$panel.background <- .merge_element(
    do.call(element_rect, args),
    object$theme$panel.background)

  object
}
