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

