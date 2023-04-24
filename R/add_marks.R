#' Add marks to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param positions Either a vector indicating the points to be highlighted or a
#'   character string with a logical expression (e.g. `values < mean(values)`)
#' @details If `positions` is an object returned from an outlier analysis
#'   ([outlier()]), the corresponding outliers are marked.
#' @return An object of class `scplot` (see[scplot()]) with changed element
#'   `marks`.
#' @examples
#' p1 <- scplot(exampleA1B1A2B2$Moritz) %>% add_marks(positions = c(1,5,10,14))
#' p1 <- scplot(Huber2014) %>% add_marks(positions = outlier(Huber2014))
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
