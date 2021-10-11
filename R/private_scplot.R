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
  x <- x / ggplot2:::.pt
  x

}

.merge_element <- function(new, old) {

  #class(new) <- class(old)
  #modifyList(new, old, keep.null = TRUE)
  merge_element(new, old)

}

.merge_theme <- function(new, old) {

  out <- old

  ids <- which(!(names(new) %in% names(old)) | !sapply(new, is.list))

  out[names(new)[ids]] <- new[ids]

  if (length(ids) > 1) new <- new[-ids]

  for(i in seq_along(new)) {
    label <- names(new)[i]
    if ("element" %in% class(out[[label]])) {
      out[[label]] <- merge_element(new[[i]], out[[label]])
    } else if ("list" %in% class(out[[label]])) {
      out[[label]] <- modifyList(out[[label]], new[[i]])
    }
  }

  out

}
