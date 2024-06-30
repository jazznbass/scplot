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
plot.sc_tauu <- function(x, effect = "A vs. B - Trend A", ...) {
  res <- lapply(x$table, \(x) x[effect, 7:9])
  res <- do.call("rbind", res)
  res <- cbind(case = row.names(res), res)
  row.names(res) <- NULL
  names(res) <- c("case", "Tau-U", "lower", "upper")
  forestplot(res, mark = 0, xlim = c(-1, 1), xlabel = effect, ...)
}



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


forestplot <- function(df,
                       title = "",
                       xlabel = names(df)[2],
                       ylabel = "",
                       xlim = NA,
                       mark = NA) {

  index <- NULL
  df$index <- 1:nrow(df)

  p <- ggplot(
    df,
    aes(
      y = index,
      x = get(names(df)[2]),
      xmin = get(names(df)[3]),
      xmax = get(names(df)[4])
    )
  )

  p <- p + geom_point() +
    geom_errorbarh(height = 0.1) +
    scale_y_continuous(breaks = 1:nrow(df), labels = df[[1]]) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())

  if (!identical(mark, NA))
    p <- p + geom_vline(xintercept = mark, color = "grey50", linetype = "dashed")

  if (!identical(xlim, NA)) p <- p + xlim(xlim[1], xlim[2])

  print(p)

}
