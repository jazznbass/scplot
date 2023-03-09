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
#' p1 <- scplot(exampleABC) %>% set_theme(my_theme)
#' @export
extract_theme <- function(object) {
  out <- structure(object$theme,
  class = "scplot-theme"
  )

  attributes(out) <- list(complete = object[c(
    "datalines", "statlines", "ridges",
    "marks", "texts", "arrows", "title", "caption", "xaxis",
    "yaxis", "xlabel", "ylabel", "labels", "phasenames", "legend",
    "casenames"
  )])

out
}
