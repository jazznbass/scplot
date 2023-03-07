#' Extract theme from an scplot object
#'
#' @inheritParams .inherit_scplot
#'
#' @return An scplot-theme object
#' @examples
#' my_theme <- scplot() %>%
#'   set_panel(color = "red") %>%
#'   set_base_text(size = 12, color = "blue") %>%
#'   set_dataline(color = "darkred", linewidth = 2) %>%
#'   extract_theme()
#' p1 <- scplot(exampleABC) %>% add_theme(my_theme)
#' @export
extract_theme <- function(object) {
  structure(object$theme, class = "scplot-theme")
}
