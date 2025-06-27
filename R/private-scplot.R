.check_theme <- function(theme) {

  if (!theme$yaxis.title.angle %in% 0:1) stop("wrong values for ylabel angle")

  theme
}


.upperfirst <- function(x) {

  unlist(
    lapply(x, function(x)
      paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
    )
  )

}

.size <- function(x, base) {

  #if (!"rel" %in% class(x))
  x <- base * x
  x <- x / ggplot2::.pt
  x

}

.merge_element <- function(new, old) {


  id <- which(names(new) == "color")
  if (length(id) > 0) names(new)[id] <- "colour"

  if (inherits(old, "element_text")) {
    check_args(
      one_of(names(new), c(
        "family", "face", "colour", "size", "hjust", "vjust", "angle",
        "lineheight", "margin")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_text", new)
  } else if (inherits(old, "element_line")) {
    check_args(
      one_of(names(new), c(
        "colour", "linewidth", "linetype", "lineend", "arrow")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_line", new)
  } else if (inherits(old, "element_rect")) {
    check_args(
      one_of(names(new), c("fill", "colour", "linewidth", "linetype")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_rect", new)
  } else if (inherits(old, "element_point")) {
    check_args(
      one_of(names(new), c("colour", "size", "shape")
      )
    )
    if (inherits(new, "list")) new <- do.call("element_point", new)
  } else if (inherits(old, "element_blank")) {
    # do nothing
  } else {
    stop("Wrong element class")
  }

  #if (inherits(new, "list")) {
  #  new <- do.call(eval(str2lang(class(old)[1])), new)
  #}

  merge_element(new, old)
}

.merge_theme <- function(new, old) {

  names_old <- names(old)
  names_new <- names(new)

  out <- old

  replace_items <- names(new)[names_new %in% names_old]

  for(i in replace_items) {

    if (inherits(old[[i]], "list")) {
      out[[i]] <- .merge_theme(new[[i]], old[[i]])
    } else if (inherits(old[[i]], "element")) {
      out[[i]] <- ggplot2::merge_element(new[[i]], old[[i]])
    } else {
      if (!is.null(new[[i]])) out[[i]] <- new[[i]]
    }

    #tmp#
    # if (inherits(old[[i]], "list")) {
    #   out[[i]] <- .merge_theme(new[[i]], old[[i]])
    # } else if (inherits(new[[i]], "element_line")) {
    #   out[[i]] <- element_blank()
    # } else if (inherits(old[[i]], "element")) {
    #   class_old <- class(old[[i]])
    #
    #   #old[[i]] <- as_list_s7(old[[i]])
    #   #new[[i]] <- as_list_s7(new[[i]])
    #   #tmp# class(old[[i]]) <- "list"
    #   #tmp# class(new[[i]]) <- "list"
    #   out[[i]] <- .merge_theme(new[[i]], old[[i]])
    #   class(out[[i]]) <- class_old
    # } else {
    #   if (!is.null(new[[i]])) out[[i]] <- new[[i]]
    # }
  }

  new_items <- names(new)[!names_new %in% names_old]
  out[new_items] <- new[new_items]

  out
}

# as_list_s7 <- function(x) {
#   if (inherits(x, "S7_object")) return(S7::props(x))
#   class(x) <- "list"
#   x
# }

