#' Extract theme from an scplot object
#'
#' @inheritParams .inherit_scplot
#'
#' @return An scplot-theme object
#' @export
extract_theme <- function(object) {
  structure(object$theme, class = "scplot-theme")
}
