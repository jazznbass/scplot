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

  # set x/y label

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

  p <- ggplot(data = as.data.frame(data), aes(x = !!sym(mvar), y = !!sym(dvar)))
  p <- p + theme(plot.margin = theme$plot.margin)
  # set axis limits  --------

  #p <- p + xlim(xlim[1], xlim[2])
  #p <- p + ylim(ylim[1], ylim[2])

  #p <- p + expand_limits(x = xlim, y=ylim)

  p <- p + scale_x_continuous(breaks = xlim[1]:xlim[2], limits = c(xlim[1],xlim[2]))

  # add dataline ---------------------------

  p <- p + geom_line(aes(group = !!sym(pvar)), col =  "black")

  # add value labels ---------------------------

  if (!is.null(theme$labels.text)) {
    p <- p + geom_label(
      aes(label = !!sym(dvar), y = !!sym(dvar)),
      colour =  theme$labels.text$colour,
      size = theme$labels.text$size,
      hjust = theme$labels.text$hjust,
      vjust = theme$labels.text$vjust,
      #lineheight = theme$labels.text$lineheight,
      #family = theme$labels.text$family,
      #fontface = theme$labels.text$face,
      #angle = theme$labels.text$angle,
      fill = theme$labels.box$fill,
      nudge_x = theme$labels.nudge_x,
      nudge_y = theme$labels.nudge_y,

    )
  }
  # add datapoints ---------------------------

  p <- p + geom_point()

  # create facets --------------------

  p <- p + facet_grid(case ~ ., scales = "free")
  #p$facet$params$free$y <- TRUE
  p <- p + theme(panel.spacing.y = unit(2, "lines"))

  p <- p + theme(strip.text.y = element_blank())

  # add casenames ------------------

  data_casenames <- data.frame(
    x = rep(xlim[1], N),
    y = rep(ylim[2], N),
    case = names(data)
  )

  p <- p + geom_text(data = data_casenames, mapping = aes(x = x, y = y, label =  case), vjust = 1, hjust = 0)

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
  if (!is.null(object$xlabel)) p <- p + xlab(object$ylabel)

  p <- p + theme(axis.title.y = theme$axis.title.y)
  p <- p + theme(axis.title.x = theme$axis.title.x)


  # out -----------

  p
}

