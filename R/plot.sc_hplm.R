#' This function generates a forest plot for the random effects of a mixed hplm model
#'
#' @param x The return from the `hplm()` function.
#' @param effect The specific effect to be plotted (default is the intercept).
#' @param mark Set a reference line.
#' @param ci Value between 0 and 1 for calculating the confidence interval.
#' @param ... Further arguments.
#' @return A forest plot displaying Tau-U effects.
#'
#' @examples
#' # plot(hplm(Leidig2018), effect = "intercept")
#'
#' @export
plot.sc_hplm <- function(x,
                         effect = "intercept",
                         mark = "mean",
                         ci = 0.95,
                         ...) {
  res <- coef(x, casewise = TRUE)
  res_fixed <- coef(x)

  z <- qnorm((1 - ci) /2, lower.tail = FALSE)
  message(
    "Possible effects are: ",
    paste0(1:nrow(res_fixed), ": '", rownames(res_fixed), "'", collapse = ", ")
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
  se <- res_fixed[effect - 1, 2]
  xlabel <- names(res)[effect]

  if (identical(mark, "mean")) mark <- mean(res[[column]], na.rm = TRUE)


  res <- res[c(1, column)]
  res$lower <- res[[2]] - z * se
  res$upper <- res[[2]] + z * se
  row.names(res) <- NULL
  names(res) <- c("case", xlabel, "lower", "upper")
  forestplot(res, xlabel = xlabel, mark = mark, ...)
}
