#' Add a theme of to an scplot
#'
#' Possible themes are: `'basic', 'grid', 'default', 'small', 'tiny', 'big',
#' 'minimal', 'dark', 'sienna', 'phase_color', 'phase_shade', 'grid2'`.
#'
#' @inheritParams .inherit_scplot
#' @param theme A character string with a predefined graphical theme or a theme
#'   object created with [new_theme()].
#' @param ... Further character strings or `scplot-theme` objects that are "added"
#'   on top.
#' @return An object of class `scplot` (see[scplot()]) with a changed `theme`
#'   element.
#' @export
set_theme <- function(object, theme, ...) {

  theme_list <- list(theme, ...)

  for(i in seq_along(theme_list)) {

    if (inherits(theme_list[[i]], "scplot-theme")) {
      theme_list[[i]] <- theme_list[[i]]$theme
    }

    if(inherits(theme_list[[i]], "character")) {
      theme_list[[i]] <- .scplot_themes[[theme_list[[i]]]]
      if(is.null(theme_list[[i]])) {
        stop(
          "Unknown theme template. Available themes are: ",
          paste0("'", names(.scplot_themes), "'", collapse = ", ")
        )
      }
    }
   }

  for(i in theme_list) object$theme <- .merge_theme(i, object$theme)

  object

}

#' @rdname set_theme
#' @export
add_theme <- function(...) {
  warning("Deprecated. Please use `set_theme()`")
  set_theme(...)
}

#' Set a theme element
#'
#' @inheritParams .inherit_scplot
#' @param ... various style parameter
#' @details Usually, you don't need this function. Possible theme elements are:
#'   "text", "plot.background", "panel.background", "panel.spacing.y",
#'   "dataline", "datapoint", "statline", "axis.expand.x", "axis.expand.y",
#'   "axis.line.x", "axis.line.y", "axis.ticks.length", "axis.ticks",
#'   "axis.title.y", "axis.title.x", "axis.text.x", "axis.text.y", "plot.title",
#'   "plot.caption", "plot.margin", "casenames", "casenames.strip",
#'   "casenames.background", "casenames.position", "phasenames",
#'   "phasenames.position.x", "separators", "separators.extent", "label.text",
#'   "label.background", "label.padding", "grid", "legend.position",
#'   "legend.background", "legend.text", "legend.title", "legend.margin".
#'
#'   The elements are of the following classes:
#'
#'   - text = c("element_text", "element"),
#'   - plot.background = c("element_rect", "element"),
#'   - panel.spacing.y = c("simpleUnit", "unit", "unit_v2"),
#'   - dataline = "list",
#'   - datapoint = "list",
#'   - statline = c("element_line", "element" ),
#'   - axis.expand.x = "numeric",
#'   - axis.expand.y = "numeric",
#'   - axis.line.x = c("element_line", "element"),
#'   - axis.line.y = c("element_line", "element"),
#'   - axis.ticks.length = c("simpleUnit", "unit", "unit_v2"),
#'   - axis.ticks = c("element_line", "element"),
#'   - axis.title.y = c("element_text", "element"),
#'   - axis.title.x = c("element_text", "element"),
#'   - axis.text.x = c("element_text", "element"),
#'   - axis.text.y = c("element_text", "element"),
#'   - plot.title = c("element_text", "element"),
#'   - plot.caption = c("element_text", "element"),
#'   - plot.margin = c("margin", "simpleUnit", "unit","unit_v2"),
#'   - casenames = c("element_text", "element"),
#'   - casenames.strip = c("element_rect", "element"),
#'   - casenames.background = c("element_rect", "element"),
#'   - casenames.position = "character",
#'   - phasenames = c("element_text", "element"),
#'   - phasenames.position.x = "character",
#'   - separators = c("element_line", "element"),
#'   - separators.extent = "character",
#'   - label.text = c("element_text", "element"),
#'   - label.background = c("element_rect", "element" ),
#'   - label.padding = "numeric", grid = c("element_line", "element" ),
#'   - legend.position = "character",
#'   - legend.background = c("element_rect",  "element"),
#'   - legend.text = c("element_text", "element"),
#'   - legend.title = c("element_text", "element"),
#'   - legend.margin = c("margin", "simpleUnit", "unit", "unit_v2")
#' @return An object of class `scplot` (see[scplot()]) with a changed `theme`
#'   element.
#' @examples
#' theme <- scplot(exampleABC) %>%
#' set_theme_element(
#'   axis.ticks.length = unit(0, "points"),
#'   axis.line.y = element_line(color = "darkred", linewidth = 2),
#'   panel.background = element_rect(color = "darkblue", linewidth = 1),
#'   panel.spacing.y = unit(0, "points"),
#'   phasenames = element_text(color = "#00000000")
#' ) %>% extract_theme()
#' @export
set_theme_element <- function(object, ...) {

  object$theme <- .merge_theme(list(...), object$theme)
  object
}
