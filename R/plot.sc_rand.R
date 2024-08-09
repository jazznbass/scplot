#' Plot Randomization Effects
#'
#' This function generates a forest plot of Tau-U effects.
#'
#' @param x The return from the `tau_u()` function.
#' @param type Either `"hist"` or `"xy"`.
#' @param add_density_curve If TRUE, adds a density curve to the histogram.
#' @param ... Further arguments.
#' @return A forest plot displaying Tau-U effects.
#'
#' @examples
#' res <- rand_test(exampleAB$Anja, limit = 1)
#' plot(res, type = "hist")
#'
#' plot(res, type = "xy")
#'
#' @export
plot.sc_rand <- function(x,
                         type = "hist",
                         add_density_curve = TRUE,
                         ...) {

  Distribution <- count <- NULL

  object <- x

  if (type == "xy") {

    ylab <- object$statistic
    xlab <- "Start phase B"
    dat <- data.frame(
      "Start phase B" = object$distribution_startpoints[[1]],
      Distribution = object$distribution,
      check.names = FALSE
    )

    col <- sym(names(dat)[1])

    p <- ggplot(dat, aes(x = !!col, y = Distribution)) +
      ylab(ylab) +
      xlab(xlab) +
      geom_point(aes(color = "darkgreen")) +
      geom_point(
        data = dat[which(dat$Distribution > object$observed.statistic),],
        aes(color = "red")
      ) +
      geom_point(
        data = dat[which(dat$Distribution == object$observed.statistic),],
        aes(color = "blue")
      ) +
      scale_x_continuous(
        breaks = min(dat[[1]]):max(dat[[1]]),
        limits = c(min(dat[[1]]), max(dat[[1]]))
      ) +
      scale_color_identity(
        name = "",
        breaks = c("darkgreen", "red", "blue"),
        labels = c("Below", "Above", "Equal"),
        guide = "legend"
      )

    return(p)
  }


  if (type == "hist") {
    dat <- data.frame(x = object$distribution)
    h <- hist(object$distribution, plot = FALSE)

    p <- ggplot(dat, aes(x)) +
      geom_histogram(
        aes(y = after_stat(density)),
        color = "black", fill = "white",
        breaks = h$breaks
      )

    if (add_density_curve) p <- p + geom_density(alpha = .2, fill = "lightgrey")

    p <- p + geom_vline(
        aes(xintercept = object$observed.statistic),
        color = "grey", linetype = "dashed", linewidth = 1
    ) +
    stat_bin(
      aes (
        y = after_stat(density),
        label= paste0(
          "n = ", after_stat(count), "\n", after_stat(round(count / sum(count) * 100)),"%")
      ),
      geom="text", breaks = h$breaks, center = 0.5, vjust = -.5
    ) +
    labs(x = object$statistic)

    p <- p + annotate(
      "text",
      y = layer_scales(p)$y$range$range[2],
      x = object$observed.statistic,
      label = "observed",
      hjust = if (object$p.value < 0.5) 1.1 else 0.1
    )

    return(p)
  }

}
