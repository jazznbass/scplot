#' Split Treatment Dataline
#'
#' This function takes a scplot object and adds treatment datalines according to
#' the specified variables.
#'
#' The function identifies unique treatment levels from the specified treatment
#' variable and creates new datalines for each level. The new datalines are
#' added to the scplot object, and the original dependent variable can
#' optionally be removed from cases where the treatment variable matches the
#' level.
#'
#' @param object A scplot object containing the scdf data.
#' @param tvar The treatment variable.
#' @param dvar The dependent variable which provides the values for the new
#'   datalines. If NULL, the default dependent variable of the scplot object is
#'   used.
#' @param levels Optional, a vector of treatment levels.
#' @param labels Optional, labels for the treatment levels.
#' @param remove_original Logical, should the original dependent variable be
#'   removed from the cases where the treatment variable matches the level?
#'   (default is FALSE).
#' @param color Optional, a vector of colors for the new datalines. If provided,
#'  the length of the color vector should match the number of treatment levels.
#'  If not provided, default colors will be used for the datalines.
#' @param ... Additional arguments.
#' @return The modified scplot object with added datalines.
#' @export
#' @examples
#' scplot(scan::example_atd) |>
#'   split_dataline("treatment")
split_dataline <- function(object,
                           tvar,
                           dvar = NULL,
                           levels = NULL,
                           labels = NULL,
                           remove_original = TRUE,
                           color = NULL,
                           ...) {

  treatment_levels <- if (is.null(levels)) {
    lapply(object$scdf, function(df) unique(df[[tvar]])) |> unlist() |> unique()
  } else {
    levels
  }
  treatment_levels <- treatment_levels[!is.na(treatment_levels)]

  if (is.null(dvar)) dvar <- object$dvar

  if (is.null(labels)) {
    labels <- paste("treatment_", treatment_levels, sep = "")
  } else if (length(labels) != length(treatment_levels)) {
    abort("Length of labels must match the number of treatment levels.")
  }

  for (level in seq_along(treatment_levels)) {
    for(case in 1:length(object$scdf)) {
      dv_column <- object$scdf[[case]][[dvar]]
      object$scdf[[case]][[labels[level]]] <- ifelse(
        object$scdf[[case]][[tvar]] == level, dv_column, NA)
      if (remove_original) {
        .filter <- sapply(object$scdf[[case]][[tvar]],
                          function(x) isTRUE(x == level))
        object$scdf[[case]][[dvar]] <- ifelse(.filter, NA, dv_column)
      }
    }
  }

  # Add datalines for each treatment level
  if (!is.null(color) && length(color) != length(treatment_levels)) {
    color <- rep(color, length.out = length(treatment_levels))
  }
  for (level in seq_along(treatment_levels)) {
    if (!is.null(color)) {
      object <- set_dataline(
        object,
        variable = labels[level],
        show_gaps = FALSE,
        color = color[level],
        point = color[level],
        ...
      )
    } else {
      object <- set_dataline(
        object,
        variable = labels[level],
        show_gaps = FALSE,
        ...
      )
    }
  }
  return(object)

}

