#' Check theme object
#'
#' Validates specific properties of a theme object.
#'
#' @param theme A theme object.
#' @return The checked theme object.
#' @keywords internal
.check_theme <- function(theme) {

  if (!theme$yaxis.title.angle %in% 0:1)
    abort("wrong values for ylabel angle")

  theme
}

#' Capitalize first letter of a string
#'
#' @param x A character vector.
#' @return A character vector with first letter capitalized.
#' @keywords internal
.upperfirst <- function(x) {

  unlist(
    lapply(x, function(x)
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    )
  )

}

#' Convert relative size to absolute size
#'
#' @param x A size value. If relative size, use `rel()` function.
#' @param base Base size to convert relative size to absolute size.
#' @return Absolute size value.
#' @keywords internal
.size <- function(x, base) {

  #if (!"rel" %in% class(x))

  x <- base * x
  x <- x / ggplot2::.pt
  x

}

#' Merge two theme elements
#'
#' @param new A list or theme element with new values.
#' @param old A theme element to be updated.
#' @return A merged theme element.
#' @keywords internal
.merge_element <- function(new, old) {

  if (is.null(new) && is.null(old)) return(NULL)
  if (is.null(old)) return(new)
  if (is.null(new)) return(old)

  id <- which(names(new) == "color")
  if (length(id) > 0) names(new)[id] <- "colour"

  if (inherits(old, "ggplot2::element_text")) {
    check_args(
      one_of(names(new), c(
        "family", "face", "colour", "size", "hjust", "vjust", "angle",
        "lineheight", "margin")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_text", new)
  } else if (inherits(old, "ggplot2::element_line")) {
    check_args(
      one_of(names(new), c(
        "colour", "linewidth", "linetype", "lineend", "arrow")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_line", new)
  } else if (inherits(old, "ggplot2::element_rect")) {
    check_args(
      one_of(names(new), c("fill", "colour", "linewidth", "linetype")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_rect", new)
  } else if (inherits(old, "ggplot2::element_point")) {
    check_args(
      one_of(names(new), c("colour", "size", "shape")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_point", new)
  } else if (inherits(old, "ggplot2::element_blank")) {
    # do nothing
  } else {
    abort("Wrong element class")
  }

  merge_element(new, old)
}

#' Merge two theme lists
#'
#' Merges two theme lists, updating the old theme with new values from the new theme.
#'
#' @param new A theme list with new values.
#' @param old A theme list to be updated.
#' @return A merged theme list.
#' @keywords internal
.merge_theme <- function(new, old) {

  names_old <- names(old)
  names_new <- names(new)

  out <- old

  replace_items <- names(new)[names_new %in% names_old]

  for(i in replace_items) {

    if (inherits(old[[i]], "list")) {
      out[[i]] <- .merge_theme(new[[i]], old[[i]])
    } else if (inherits(old[[i]], c("element", "ggplot2::element"))) {
      out[[i]] <- ggplot2::merge_element(new[[i]], old[[i]])
    } else {
      if (!is.null(new[[i]])) out[[i]] <- new[[i]]
    }
  }

  new_items <- names(new)[!names_new %in% names_old]
  out[new_items] <- new[new_items]

  out
}

