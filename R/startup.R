.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(
    "\033[31m", "scplot ",  packageVersion("scplot"),
    " (", packageDate("scplot"), ")",
    "\033[34m"
  )

}

.onLoad <- function(lib, pkg, ...) {

  new_options <- list(
    "scplot.plot.theme" = c("basic", "grid3"),
    "scplot.plot.caption" = "auto"
  )

  options(new_options)
}
