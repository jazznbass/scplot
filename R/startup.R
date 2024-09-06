.onLoad <- function(lib, pkg, ...) {

  new_options <- list(
    "scplot.plot.theme" = "default",
    "scplot.plot.caption" = "auto"
  )

  options(new_options)
}
