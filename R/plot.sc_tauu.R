#' Plot Tau-U Effects
#'
#' This function generates a forest plot of Tau-U effects.
#'
#' @param x The return from the `tau_u()` function.
#' @param effect The specific effect to be plotted (default is "A vs. B - Trend A").
#' @param ... Further arguments.
#' @return A forest plot displaying Tau-U effects.
#'
#' @examples
#' # plot(tau_u(Leidig2018), effect = "A vs. B - Trend A")
#'
#' @export
plot.sc_tauu <- function(x, effect = 1, ...) {

  if (is.character(effect)) {
    if (!effect %in% rownames(x$table[[1]])) {
      message(
        "Possible effects are:\n",
        paste0(1:nrow(x$table[[1]]), ": '", rownames(x$table[[1]]), "'", collapse = "\n")
      )
    }
  }

  if (is.numeric(effect)) {
    effect <- rownames(x$table[[1]])[effect]
  }

  res <- lapply(x$table, \(x) x[effect, 7:9])
  res <- do.call("rbind", res)
  res <- cbind(case = row.names(res), res)
  row.names(res) <- NULL
  names(res) <- c("case", "Tau-U", "lower", "upper")
  forestplot(res, mark = 0, xlim = c(-1, 1), xlabel = effect, ...)
}
