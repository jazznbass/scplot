#' Add a theme of to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param theme A character string with a predefined graphical theme.
#' @export
add_theme <- function(object, theme, ...) {

  theme_list <- list(theme, ...)

  for(i in seq_along(theme_list)) {

    if(inherits(theme_list[[i]], "character")) {
      theme_list[[i]] <- .scplot_themes[[theme_list[[i]]]]
      if(is.null(theme_list[[i]])) {
        stop(
          "Unknown theme template. ",
          "Available themes are: ",
          paste0("'", names(.scplot_themes), "'", collapse = ", ")
        )
      }
    }
   }

  for(i in theme_list)
   object$theme <- .merge_theme(i, object$theme)

  object

}

