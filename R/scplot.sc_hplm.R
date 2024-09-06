#' This function generates a forest plot for the random effects of a mixed hplm
#' model
#'
#' @param object The return from the `hplm()` function.
#' @param effect The specific effect to be plotted (default is the intercept).
#' @param mark Set a reference line.
#' @param ci Value between 0 and 1 for calculating the confidence interval.
#' @param ... Further arguments.
#' @return A forest plot displaying Tau-U effects.
#'
#' @examples
#' model <- scan::hplm(scan::Leidig2018, random.slopes = TRUE)
#' scplot(model, effect = "level")
#'
#' @export
scplot.sc_hplm <- function(object,
                         effect = "intercept",
                         mark = "fixed",
                         ci = 0.95,
                         ...) {

  x <- object
  coef_random <- coef(x, casewise = TRUE)
  coef_fixed <- coef(x)

  z <- qnorm((1 - ci) /2, lower.tail = FALSE)

  message(
    "Possible effects are: \n",
    paste0(
      2:ncol(coef_random), ": '", names(coef_random)[-1], "'",
      collapse = "\n"
    )
  )
  if (is.character(effect)) {
    effect <- switch (
      effect,
      "intercept" = 2,
      "trend" = 3,
      "level" = 4,
      "slope" = 5
    )
  }

  column <- effect
  se <- coef_fixed[effect - 1, 2]
  xlabel <- names(coef_random)[effect]

  if (identical(mark, "fixed"))
    mark <- mean(coef_random[[column]], na.rm = TRUE)

  out <- coef_random[c(1, column)]
  out$lower <- out[[2]] - z * se
  out$upper <- out[[2]] + z * se
  row.names(out) <- NULL
  names(out) <- c("case", xlabel, "lower", "upper")
  footnote <- paste0(
    "Note. The dashed line indicates the fixed effect and \n",
    "the errorbars indicate the ",
    ci * 100, "% confidence intervall."
  )
  title <- paste0("Casewise effects calculated from the \n",
                  "random slope effect of a multilevel model")
  forestplot(
    out, xlabel = xlabel, mark = mark, footnote = footnote, title = title,
    ...
  )
}
