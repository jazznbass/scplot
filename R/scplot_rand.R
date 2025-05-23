#' Random start position plot
#' Plot of statistics for random phase B start positions
#' @param scdf A single-case data frame object.
#' @param statistic A string with a the name of a statistic.
#'   Defaults to `Mean B-A`. See rand_test() function for all options.
#' @param x_label Character string with the x label.
#' @param color_label Character string with the color label.
#' @param colors Named vector with color codes.
#' @param ... further arguments passted to the scan `rand_test()` function.
#' @examples
#' scplot_rand(scan::byHeart2011[1:5])
#'
#' @export
scplot_rand <- function(scdf,
                        statistic = "Mean B-A",
                        x_label = "Start phase B",
                        color_label = "Compared to\nobserved",
                        colors = c(
                          Above = "coral3", Below = "aquamarine4",
                          Observed = "#56B4E9", Equal = "black"
                        ),
                        ...) {

  Distribution <- NULL
  object <- vector("list", length(scdf))

  for(i in 1:length(object)) {
    object[[i]] <- scan::rand_test(
      data = scdf[i],
      statistic = statistic,
      limit = 1,
      complete = TRUE,
      ...
    )
  }

  ylab <- object[[1]]$statistic

  dat <- lapply(object, function(x) {
    out <- data.frame(
      case = attr(x, "casenames"),
      "Start phase B" = x$distribution_startpoints[[1]],
      Distribution = x$distribution,
      check.names = FALSE
    )
    out[[color_label]] <- ifelse(
      out[[3]] < x$observed.statistic, names(colors)[2],
      ifelse(
        out[[3]] > x$observed.statistic, names(colors)[1], names(colors)[4]
      )
    )
    out[[color_label]][x$n1] <- names(colors)[3]
    out
  })

  dat <- do.call(rbind, dat)

  xlab <- sym(names(dat)[2])
  col <- sym(names(dat)[4])

  p <- ggplot(dat, aes(x = !!xlab, y = Distribution, colour = !!col)) +
    ylab(ylab) +
    xlab(xlab) +
    geom_point() +
    geom_vline(
      xintercept = sapply(object, function(x) x$n1 + 0.5),
      color = "grey", linetype = "dashed", linewidth = 1
    ) + scale_x_continuous(
      breaks = min(dat[[2]]):max(dat[[2]]),
      limits = c(min(dat[[2]]), max(dat[[2]]))
    )

  p <- p + theme_bw()
  p <- p + scale_color_manual(values = colors)

  p <- p + facet_wrap(~case, nrow = length(object), strip.position = "right")


  return(p)
}

