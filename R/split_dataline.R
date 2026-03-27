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
#' @param auto_add Logical, should the new columns be added automatically?
#'   (default is TRUE)
#' @param remove_original Logical, should the original dependent variable be
#'   removed from the cases where the treatment variable matches the level?
#'   (default is FALSE)
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
                           auto_add = TRUE,
                           remove_original = TRUE,
                           ...) {

  treatment_levels <- if (is.null(levels)) {
    lapply(object$scdf, function(df) unique(df[[tvar]])) |> unlist() |> unique()
  } else {
    levels
  }
  treatment_levels <- treatment_levels[!is.na(treatment_levels)]

  if (is.null(dvar)) dvar <- object$dvar

  prefix <- "treatment_"

  if (is.null(labels)) {
    labels <- paste(prefix, treatment_levels, sep = "")
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

  if (auto_add) {

    for (level in seq_along(treatment_levels)) {
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

