#' Add a legend to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param position The position ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param datalines If TRUE, a legend for the datalines is generated.
#' @param statlines If TRUE, a legend for the statlines is generated.
#' @param title A list with text style parameters for the title.
#' @export
add_legend <- function(object,
                       labels = NULL,
                       section_labels = c("Lines", "Phases"),
                       case = 1,
                       position = "right",
                       datalines = TRUE,
                       statlines = TRUE,
                       title = NULL,
                       text = NULL,
                       background = NULL
) {

  object$legend$labels <- labels
  object$legend$section_labels <- section_labels
  object$legend$statlines <- statlines
  object$legend$datalines <- datalines


  object$theme$legend.title <-
    .merge_element(title, object$theme$legend.title)

  object$theme$legend.text <-
    .merge_element(text, object$theme$legend.text)

  object$theme$legend.background <-
    .merge_element(background, object$theme$legend.background)

  object$theme$legend.position <- position

  object
}
















