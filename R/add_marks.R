#' Add marks to an scplot
#'
#' @inheritParams .inherit_scplot
#' @param positions Either a vector indicating the dots to be highlighted or a
#' character string with a logical expression (e.g. values < mean(values))
#' @export
add_marks <- function(object,
                      case = 1,
                      positions,
                      color = "red",
                      size = 1,
                      shape = 1,
                      variable = ".dvar") {

  # Marks on the outliers from outlier()
  if (identical(class(positions), "sc_outlier")) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks,
        list(
          list(
            case = i, positions = positions$dropped.mt[[i]],
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
