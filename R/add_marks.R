#' Add marks to an scplot object
#'
#' Marks specific points in an scplot object.
#'
#' Marks are visualized as points on top of the existing plot. Multiple marks
#' can be added by calling `add_marks()` multiple times.
#'
#' @inheritParams .inherit_scplot
#' @param positions Either a vector indicating the points to be highlighted or a
#'   character string with a logical expression (e.g. `values < mean(values)`)
#'   indicating the points to be highlighted. Alternatively, an object of class
#'   `sc_outlier` returned from the [outlier()] function can be used
#'   to mark the detected outliers. A list of such vectors or expressions can
#'   also be provided to add marks for multiple cases at once.
#' @details If `positions` is an object returned from an outlier analysis via
#'   `outlier()`, the corresponding outliers are marked. If `positions` is a
#'   list, marks are added for each case in the list.
#'   The `variable` argument specifies the variable on which the marks are
#'   applied. By default, the variable `.dvar` is used. If multiple cases are
#'   plotted, the `case` argument specifies for which case(s) the marks are
#'   added.
#' @return An object of class `scplot` (see [scplot()]) with changed element
#'   `marks`.
#' @author Juergen Wilbert
#' @examples
#' library(scan)
#' p1 <- scplot(exampleA1B1A2B2$Moritz) |> add_marks(positions = c(1,5,10,14))
#' p1 <- scplot(Huber2014) |> add_marks(positions = outlier(Huber2014))
#' @export
add_marks <- function(object,
                      case = 1,
                      positions,
                      color = "red",
                      size = 1,
                      shape = 1,
                      variable = ".dvar") {

  # Marks on the outliers from outlier()
  if (inherits(positions, "sc_outlier")) {
    positions <- positions$dropped.mt
  }

  if (is.list(positions)) {
    for(i in seq_along(positions))
      object$marks <- c(
        object$marks,
        list(
          list(
            case = i, positions = positions[[i]],
            color = color, size = size, shape = shape, variable = variable
          )
        )
      )
    return(object)
  }

  for(i in case) {
    object$marks <- c(
      object$marks,
      list(
        list(
          case = i, positions = positions,
          color = color, size = size, shape = shape, variable = variable
        )
      )
    )
  }

  object
}
