#' Random start position plot
#' Plot of statistics for random phase B start positions
#' @param scdf A single-case data frame object.
#' @param statistic A string with a the name of a statistic.
#'   Defaults to `Mean B-A`. See rand_test() function for all options.
#' @param ... further arguments passted to the scan `rand_test()` function.
#' @examples
#' scplot_stats(scan::byHeart2011[1:5])
#'
#' @export
scplot_rand <- function(scdf,
                        statistic = "Mean B-A",
                        colors = c("coral3", "aquamarine4", "#56B4E9", "black"),
                        ...) {

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
  xlab <- "Start phase B"

  dat <- lapply(object, function(x) {
    out <- data.frame(
      case = attr(x, "casenames"),
      "Start phase B" = x$distribution_startpoints[[1]],
      Distribution = x$distribution,
      check.names = FALSE
    )
    out$'Compared to observed' <- ifelse(out[[3]] < x$observed.statistic, "Below",
                        ifelse(out[[3]] > x$observed.statistic, "Above", "Equal"))
    out$'Compared to observed'[x$n1] <- "Observed"
    out
  })

  dat <- do.call(rbind, dat)

  col <- sym(names(dat)[2])

  p <- ggplot(dat, aes(x = !!col, y = Distribution, colour = `Compared to observed`)) +
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

