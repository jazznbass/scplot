#' Add a ridge to an scplot
#'
#' @inheritParams .inherit_scplot
#' @return An object of class `scplot` (see[scplot()]) with changed element
#'   `ridges`.
#' @export
add_ridge <- function(object,
                      color = "grey98",
                      variable = ".dvar") {

  new_ridge <- list(
    variable = variable,
    colour = color
  )

  object$ridges <- c(object$ridges, list(new_ridge))
  object
}
