#' print.scplot
#' ggplot2 graphic plots from scdf files
#'
#' @param object An scplot object
#'
#' @return A ggplot2 object
#' @export
#' @import scan
#' @import ggplot2
print.scplot <- function(x, ...) {

  object <- x

  data <- object$scdf

  #rename casesnames
  names(data) <- object$casenames$labels

  data_long <- as.data.frame(data)
  data_long$case <- factor(data_long$case, levels = object$casenames$labels)

  theme <- object$theme

  dvar <- object$dvar
  pvar <- object$pvar
  mvar <- object$mvar

  .design <- function(data, pvar, mvar) {
    phases <- rle(as.character(data[[pvar]]))
    phases$start <- c(1, cumsum(phases$lengths) + 1)[1:length(phases$lengths)]
    phases$stop  <- cumsum(phases$lengths)
    phases$start_mt <- data[[mvar]][phases$start]
    phases$stop_mt <- data[[mvar]][phases$stop]
    class(phases) <- "list"
    phases
  }

  design <- lapply(data, .design, pvar = pvar, mvar = mvar)

  N <- length(data)

  # set dataline for first dvar

  if (is.null(object$datalines[[1]]$variable))
    object$datalines[[1]]$variable <- object$dvar[1]

  if (is.null(object$datalines[[1]]$col))
    object$datalines[[1]]$col <- theme$dataline.col

  if (is.null(object$datalines[[1]]$width))
    object$datalines[[1]]$width <- theme$dataline.width

  if (is.null(object$datalines[[1]]$linetype))
    object$datalines[[1]]$linetype <- theme$dataline.linetype

  if (is.null(object$datalines[[1]]$dots))
    object$datalines[[1]]$dots <- theme$datadots.col

  if (is.null(object$datalines[[1]]$shape))
    object$datalines[[1]]$shape <- theme$datadots.shape

  if (is.null(object$datalines[[1]]$size))
    object$datalines[[1]]$size <- theme$datadots.size

  # set x/y label --------

  if (is.null(object$xlabel)) {
    object$xlabel <- mvar
    if (object$xlabel == "mt") object$xlabel <- "Measurement time"
    object$xlabel <- .upperfirst(object$xlabel)
  }
  if (is.null(object$ylabel)) {
    if (length(object$dvar) == 1) {
      object$ylabel <- object$dvar
      object$ylabel <- .upperfirst(object$ylabel)
    } else {
      object$ylabel <- "Values"
    }
  }

  # compute global xlim and ylim ---------

  ylim <- object$yaxis$lim
  xlim <- object$xaxis$lim

  if (is.null(ylim)) {
    .dv <- unlist(lapply(data, function(x) x[, object$dvar]))
    ylim <- c(min(.dv, na.rm = TRUE), max(.dv, na.rm = TRUE))
  }

  if (is.null(xlim)) {
    .mt     <- unlist(lapply(data, function(x) x[, mvar]))
    xlim <- c(min(.mt, na.rm = TRUE), max(.mt, na.rm = TRUE))
  }

  # start plot -------------

  dvar <- dvar[1]

  p <- ggplot(
    data = data_long,
    aes(x = !!sym(mvar), y = !!sym(dvar))
  )

  p <- p + theme(plot.margin = theme$plot.margin)

  # set yaxis ticks and text  --------

  if (!is.null(object$yaxis$inc)) {
    p <- p + scale_y_continuous(
      limits = c(ylim[1], ylim[2]),
      breaks = seq(ylim[1], ylim[2], object$yaxis$inc)
    )
  } else {
    p <- p + scale_y_continuous(limits = c(ylim[1], ylim[2]))
  }

  # set xaxis ticks and text  --------

  if (!is.null(object$xaxis$inc_from)) {
    x <- seq(object$xaxis$inc_from, xlim[2], object$xaxis$inc)
    x <- x[x >= xlim[1]]
    x <- c(1, x)
  } else {
    x <- seq(xlim[1], xlim[2], object$xaxis$inc)
  }

  p <- p + scale_x_continuous(
    breaks = x, #seq(xlim[1], xlim[2], object$xaxis$inc),
    limits = c(xlim[1], xlim[2])
  )

  #p <- p + xlim(xlim[1], xlim[2])
  #p <- p + ylim(ylim[1], ylim[2])
  #p <- p + expand_limits(x = xlim, y=ylim)

  # add dataline ---------------------------

  for (i in 1:length(object$datalines)) {
      p <- p + geom_line(
        aes(
          y = !!sym(object$datalines[[i]]$variable),
          group = !!sym(pvar)
        ),
        colour =  object$datalines[[i]]$col,
        size = object$datalines[[i]]$width,
        linetype = object$datalines[[i]]$linetype
      )

      # add datapoints

      if (!is.null(object$datalines[[i]]$dots)) {
        p <- p + geom_point(
          aes(y = !!sym(object$datalines[[i]]$variable)),
          colour = object$datalines[[i]]$dots,
          size = object$datalines[[i]]$size,
          shape = object$datalines[[i]]$shape,
        )
      }

  }

  # add value labels ---------------------------

  if (isTRUE(object$labels)) {

    if (is.null(theme$labels.box$fill))
      p <- p + geom_text(
        aes(label = !!sym(dvar), y = !!sym(dvar)),
        colour =  theme$labels.text$colour,
        size = theme$labels.text$size,
        hjust = theme$labels.text$hjust,
        vjust = theme$labels.text$vjust,
        #lineheight = theme$labels.text$lineheight,
        #family = theme$labels.text$family,
        #fontface = theme$labels.text$face,
        angle = theme$labels.text$angle,
        nudge_x = theme$labels.nudge_x,
        nudge_y = theme$labels.nudge_y,
      )

    if (!is.null(theme$labels.box$fill))
      p <- p + geom_label(
        aes(label = !!sym(dvar), y = !!sym(dvar)),
        colour =  theme$labels.text$colour,
        size = theme$labels.text$size,
        hjust = theme$labels.text$hjust,
        vjust = theme$labels.text$vjust,
        angle = theme$labels.text$angle,
        #lineheight = theme$labels.text$lineheight,
        fill = theme$labels.box$fill,
        nudge_x = theme$labels.nudge_x,
        nudge_y = theme$labels.nudge_y,
        label.padding = unit(theme$labels.padding, "lines")
      )
  }


  # create facets --------------------

  p <- p + facet_grid(as.factor(case) ~ ., scales = "free")
  p <- p + theme(panel.spacing.y = unit(2, "lines"))
  p <- p + theme(strip.text.y = element_blank())

  # add casenames ------------------

  if (is.null(theme$casenames.position.x)) x <- xlim[1]
    else if (theme$casenames.position.x == "left") x <- xlim[1]
    else if (theme$casenames.position.x == "right") x <- xlim[2]
    else x <- theme$casenames.position.x

  if (is.null(theme$casenames.position.y)) y <- ylim[2]
    else if (theme$casenames.position.y == "top") y <- ylim[2]
    else if (theme$casenames.position.y == "bottom") y <- ylim[1]
    else y <- theme$casenames.position.y

  data_casenames <- data.frame(
    x = rep(x, N),
    y = rep(y, N),
    case = object$casenames$labels
  )

  p <- p + geom_text(
    data = data_casenames,
    mapping = aes(x = x, y = y, label =  case),
    colour =  theme$casenames$colour,
    size = theme$casenames$size,
    hjust = theme$casenames$hjust,
    vjust = theme$casenames$vjust,
    #lineheight = theme$labels.text$lineheight,
    #family = theme$labels.text$family,
    #fontface = theme$labels.text$face,
    angle = theme$casenames$angle,
    #nudge_x = theme$casenames.nudge_x,
    #nudge_y = theme$casenames.nudge_y,
  )

  # add phaselines ----------------------------------------------------------

  data_phase <- data.frame(
    case = rep(names(design), sapply(design, function(x) length(x$stop_mt) - 1)),
    x = unlist(lapply(design, function(x) x$stop_mt[-length(x$stop_mt)] + 0.5))
  )

  p <- p + geom_vline(data = data_phase, aes(xintercept = x), linetype="dashed", color = "black", size = 0.5)

  p <- p + coord_cartesian(clip="off")

  # add phasenames ---------

  .phasenames <- lapply(design, function(x) x$values)

  x <- lapply(design, function(x) (x$stop_mt - x$start_mt) / 2 + x$start_mt)

  data_phasenames <- data.frame(
    case = rep(names(design), sapply(design, function(x) length(x$values))),
    phase = unlist(lapply(design, function(x) x$values)),
    x = unlist(x)
  )
  p <- p + geom_text(data = data_phasenames, vjust = 0, aes(label = phase, x = x, y = Inf))
  #p <- p + theme(plot.margin = margin(t = 1, 0.5, 0.5, 0.5, unit = "lines"))

  # add axis.line -----------

  #p <- p + theme(axis.line = object$theme$axis.line)

  # add grid ------------

  if (!is.null(theme$grid)) {
    p <- p + theme(panel.grid = object$theme$grid)
  }


  # add title ------------------------

  if (!is.null(object$title)) {
    p <- p + ggtitle(object$title) +
    theme(plot.title = object$theme$plot.title)
  }

  # add axis label ------

  if (!is.null(object$ylabel)) p <- p + ylab(object$ylabel)
  if (!is.null(object$xlabel)) p <- p + xlab(object$xlabel)

  p <- p + theme(axis.title.y = theme$axis.title.y)
  p <- p + theme(axis.title.x = theme$axis.title.x)


  # out -----------

  p
}

