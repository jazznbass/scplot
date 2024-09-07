#' Plot Tau-U Effects
#'
#' This function generates a forest plot of Tau-U effects.
#'
#' @param object The return from the `tau_u()` function.
#' @param effect The specific effect to be plotted (default is "A vs. B - Trend A").
#' @param ... Further arguments.
#' @return A forest plot displaying Tau-U effects.
#'
#' @examples
#' res <- scan::tau_u(scan::Leidig2018)
#' scplot(res, effect = 3)
#'
#' @export
scplot.sc_tauu <- function(object, effect = 1, ...) {

  x <- object

  message(
    "Possible effects are:\n",
    paste0(1:nrow(x$table[[1]]), ": '", rownames(x$table[[1]]), "'", collapse = "\n")
  )

  if (is.numeric(effect)) {
    effect <- rownames(x$table[[1]])[effect]
  }

  id <- which(x$Overall_tau_u$Model == effect)
  meta_value <- x$Overall_tau_u[id, "Tau_U"]

  out <- lapply(x$table, \(x) x[effect, 7:9])
  out <- do.call("rbind", out)
  out <- cbind(case = row.names(out), out)
  row.names(out) <- NULL
  names(out) <- c("case", "Tau-U", "lower", "upper")

  footnote <- paste0("Note. The red line indicates the overall tau-u value of ",
                     "the meta analysis and \nthe errorbars indicate the ",
                     x$ci * 100, "% confidence intervall.")

  forestplot(
    out, mark = c("grey" = 0, "darkred" = meta_value), xlim = c(-1, 1),
    xlabel = effect, footnote = footnote, title = "Tau-u values per case",
    ...
  )
}
