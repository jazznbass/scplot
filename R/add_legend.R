#' Add a legend to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param position The position ("none", "left", "right", "bottom", "top", or
#'   two-element numeric vector)
#' @param datalines If TRUE, a legend for the datalines is generated.
#' @param statlines If TRUE, a legend for the statlines is generated.
#' @param phases If TRUE, a legend for the phases is generated.
#'  Note that you also have to set the `set_panel` argument
#'  (e.g., `set_panel(fill = c("lightblue", "grey80"))`).
#' @param section_labels A character vector of length two. The labels for the
#'   lines section and phase section.
#' @param title A list with text style parameters for the title.
#' @return An object of class `scplot` (see[scplot()]) with changed element
#'   `legend`.
#' @examples
#' data(exampleAB_add, package = "scan")
#' scplot(exampleAB_add) |>
#'   set_dataline("depression") |>
#'   add_statline("mean") |>
#'   add_legend()
#'
#' scplot(exampleAB_add) |>
#'   set_dataline(label = "Pychological Wellbeing") |>
#'   set_dataline("depression", color = "darkblue", label = "Depression") |>
#'   add_statline("mean", label = "Wellbeing mean") |>
#'   add_statline("mean", variable = "depression", label = "Depression mean") |>
#'   set_phasenames(color = NA) |>
#'   set_panel(fill = c("lightblue", "grey80")) |>
#'   add_legend(
#'     position = "left",
#'     section_labels = c("Variables", "Section"),
#'     title = list(color = "brown", size = 10, face = 2),
#'     text = list(color = "darkgreen", size = 10, face = 2),
#'     background = list(color = "lightgrey")
#'   )
#' @export
add_legend <- function(object,
                       labels = NULL,
                       section_labels = c("Lines", "Phases"),
                       case = 1,
                       position = "right",
                       datalines = TRUE,
                       statlines = TRUE,
                       phases = TRUE,
                       title = NULL,
                       text = NULL,
                       background = NULL
) {

  if (!is.null(labels)) warning("label argument is deprecated. Please set ",
                                "label argument in the add_statline and ",
                                "set_dataline functions.")
  object$legend$labels <- labels
  object$legend$section_labels <- section_labels
  object$legend$statlines <- statlines
  object$legend$datalines <- datalines
  object$legend$phases <- phases


  object$theme$legend.title <-
    .merge_element(title, object$theme$legend.title)

  object$theme$legend.text <-
    .merge_element(text, object$theme$legend.text)

  object$theme$legend.background <-
    .merge_element(background, object$theme$legend.background)

  object$theme$legend.position <- position

  object
}
















