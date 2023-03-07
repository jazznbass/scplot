#' Add a ridge to an scplot
#'
#' @inheritParams .inherit_scplot
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
