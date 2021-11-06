#' Creates a ggplot2 object from an scplot object
#'
#' @param scplot An scplot object
#'
#' @return A ggplot2 object
#' @export
as_ggplot <- function(scplot) {

  # set global variables --------
  object <- scplot

  data <- object$scdf
  theme <- object$theme
  dvar <- object$dvar
  pvar <- object$pvar
  mvar <- object$mvar

  base_size <- theme$text$size

  N <- length(data)

  # rename casesnames --------

  names(data) <- object$casenames$labels

  # rename phasenames ----------

  if (!identical(object$phasenames$labels, ".default")) {
    for(i in seq_along(data)) {
      levels(data[[i]][[pvar]]) <- object$phasenames$labels
    }
  }

  # convert to long format --------

  data_long <- as.data.frame(data)
  data_long$case <- factor(data_long$case, levels = object$casenames$labels)
  data_long[[pvar]] <- factor(data_long[[pvar]])

  # extract design --------

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

  # set dataline for first dvar ---------------------

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
    aes(x = !!sym(mvar))
  )

  p <- p + theme_void(base_size = base_size)
  p <- p + theme(text = theme$text)
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

  p <- p + theme(axis.text.x= theme$axis.text.x)

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

  p <- p + theme(axis.text.y = theme$axis.text.y)
  #p <- p + xlim(xlim[1], xlim[2])
  #p <- p + ylim(ylim[1], ylim[2])
  #p <- p + expand_limits(x = xlim, y=ylim)

  # create facets --------------------

  p <- p + facet_grid(as.factor(case) ~ ., scales = "free")
  p <- p + theme(panel.spacing.y = theme$panel.spacing.y)
  p <- p + theme(strip.text.y = theme$casenames)
  p <- p + theme(strip.background = theme$casenames.strip)
  if (!identical(theme$casenames.position, "strip"))
    p <- p + theme(strip.text.y = element_blank())

  # add panel background ------------

  if (length(theme$panel.background$fill) > 1) {

    type_phases <- unique(data_long[[pvar]])
    color <- rep(theme$panel.background$fill, length = length(type_phases))

    theme$panel.background$fill <- "white"

    p <- p + scale_fill_manual(values = color)

    #p <- p + theme(panel.ontop = TRUE)

    x1 <- unlist(lapply(design, function(x) c(-Inf, x$start_mt[-1] - 0.5)))
    x2 <- unlist(
      lapply(design, function(x) c(x$stop_mt[-length(x$stop_mt)] + 0.5, Inf))
    )

    phase <- unlist(lapply(design, function(x) x$values))

    data_phase <- data.frame(
      case = rep(
        names(design),
        sapply(design, function(x) length(x$stop_mt))
      ),
      phase = factor(phase, levels = unique(phase)),
      x1 = x1,
      x2 = x2
    )

    p <- p + geom_rect(
      data = data_phase,
      aes(xmin = x1, xmax = x2, ymin = -Inf, ymax = Inf, fill = phase),
      inherit.aes = FALSE
    )
  }

  p <- p + theme(panel.background = theme$panel.background)

  # add ridges ---------------------------

  if (!is.null(object$ridges)) {
    for(i in seq_along(object$ridges)) {
      if (object$ridges[[i]]$variable == ".dvar")
        object$ridges[[i]]$variable <- dvar
      p <- p + geom_ribbon(
        aes(ymax = !!sym(object$ridges[[i]]$variable),
            ymin = ylim[1],
            group = !!sym(pvar)),
        fill = object$ridges[[i]]$colour
      )
    }
  }

  # add dataline and dots ---------------------------

  for (i in 1:length(object$datalines)) {
    p <- p + geom_line(
      aes(
        y = !!sym(object$datalines[[i]]$variable),
        group = !!sym(pvar),
        colour = !!object$datalines[[i]]$col
      ),
      #colour =  object$datalines[[i]]$col,
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

  if (!is.null(object$labels)) {

    for(i in seq_along(object$labels)){

      label <- object$labels[[i]]
      if (label$variable == ".dvar") label$variable <- dvar

      if (is.null(label$background$fill))
        p <- p + geom_text(
          aes(label = !!sym(label$variable), y = !!sym(label$variable)),
          colour =  label$text$colour,
          size = .size(label$text$size, base_size),
          hjust = label$text$hjust,
          vjust = label$text$vjust,
          #lineheight = theme$labels.text$lineheight,
          #family = theme$labels.text$family,
          #fontface = theme$labels.text$face,
          angle = label$text$angle,
          nudge_x = label$nudge_x,
          nudge_y = label$nudge_y
        )

      if (!is.null(label$background$fill))
        p <- p + geom_label(
          aes(label = !!sym(label$variable), y = !!sym(label$variable)),
          colour =  label$text$colour,
          size = .size(label$text$size, base_size),
          hjust = label$text$hjust,
          vjust = label$text$vjust,
          angle = label$text$angle,
          #lineheight = theme$labels.text$lineheight,
          fill = label$background$fill,
          nudge_x = label$nudge_x,
          nudge_y = label$nudge_y,
          label.padding = unit(label$padding, "lines")
        )
    }
  }

  # add casenames ------------------

  if (!identical(theme$casenames.position, "strip")) {

    if (identical(theme$casenames.position, "topleft")) {
      x <- xlim[1]
      y <- ylim[2]
    }
    if (identical(theme$casenames.position, "topright")) {
      x <- xlim[2]
      y <- ylim[2]
    }
    if (identical(theme$casenames.position, "bottomleft")) {
      x <- xlim[1]
      y <- ylim[1]
    }
    if (identical(theme$casenames.position, "bottomright")) {
      x <- xlim[2]
      y <- ylim[1]
    }

    if(length(theme$case.position) == 2) {
      x <- theme$casenames.position[1]
      y <- theme$casenames.position[2]
    }

    data_casenames <- data.frame(
      x = rep(x, N),
      y = rep(y, N),
      case = object$casenames$labels
    )

    if (is.null(theme$casenames$size)) theme$casenames$size <- 1
    theme$casenames <- merge_element(theme$casenames, theme$text)

    p <- p + geom_text(
      data = data_casenames,
      mapping = aes(x = x, y = y, label =  case),
      colour =  theme$casenames$colour,
      size = .size(theme$casenames$size, base_size),
      hjust = theme$casenames$hjust,
      vjust = theme$casenames$vjust,
      lineheight = theme$casenames$lineheight,
      family = theme$casenames$family,
      fontface = theme$casenames$face,
      angle = theme$casenames$angle
    )
  }

  # add phaselines ----------------------------------------------------------

  data_phase <- data.frame(
    case = rep(
      names(design),
      sapply(design, function(x) length(x$stop_mt) - 1)
    ),
    x = unlist(lapply(design, function(x) x$stop_mt[-length(x$stop_mt)] + 0.5))
  )

  p <- p + geom_vline(
    data = data_phase,
    aes(xintercept = x),
    linetype = theme$seperators$linetype,
    color = theme$seperators$colour,
    size = theme$seperators$size
  )

  p <- p + coord_cartesian(clip = "off")

  # add phasenames ---------

  if (theme$phasenames.position.x == "centre") {
    x <- lapply(design, function(x) (x$stop_mt - x$start_mt) / 2 + x$start_mt)
  }

  if (theme$phasenames.position.x == "left") {
    x <- lapply(design, function(x) x$start_mt)
  }

  data_phasenames <- data.frame(
    case = rep(names(design), sapply(design, function(x) length(x$values))),
    phase = unlist(lapply(design, function(x) x$values)),
    x = unlist(x)
  )

  if (is.null(theme$phasenames$size)) theme$phasenames$size <- 1
  theme$phasenames <- merge_element(theme$phasenames, theme$text)

  p <- p + geom_text(
    data = data_phasenames,
    aes(label = phase, x = x, y = Inf),
    colour =  theme$phasenames$colour,
    size = .size(theme$phasenames$size, base_size),
    hjust = theme$phasenames$hjust,
    vjust = theme$phasenames$vjust,
    family = theme$phasenames$family,
    fontface = theme$phasenames$face,
    angle = theme$phasenames$angle,
    lineheight = theme$phasenames$lineheight
  )

  # add axis.line -----------

  p <- p + theme(axis.line.x = theme$axis.line.x)
  p <- p + theme(axis.line.y = theme$axis.line.y)
  p <- p + theme(axis.ticks.length = theme$axis.ticks.length)
  p <- p + theme(axis.ticks = theme$axis.ticks)


  #axis.ticks


  # add grid ------------

  #if (!is.null(theme$grid$colour)) {
    p <- p + theme(panel.grid = theme$grid)
  #}


  # add title ------------------------

  if (!is.null(object$title)) {
    p <- p + ggtitle(object$title) +
      theme(plot.title = theme$plot.title)
  }

  # add caption -------------

  if (!is.null(object$caption)) {
    p <- p + labs(caption = object$caption) +
      theme(plot.caption = theme$plot.caption, plot.caption.position = "plot")

  }

  # add axis label ------

  if (!is.null(object$ylabel)) p <- p + ylab(object$ylabel)
  if (!is.null(object$xlabel)) p <- p + xlab(object$xlabel)

  p <- p + theme(axis.title.y = theme$axis.title.y)
  p <- p + theme(axis.title.x = theme$axis.title.x)

  # add statlines ------------------------------------------------------------

  if (!is.null(object$statlines)) {

    for(j in 1:length(object$statlines)) {
      if (object$statlines[[j]]$variable == ".dvar")
        object$statlines[[j]]$variable <- object$dvar[1]

      possible_fixed_stats <- c("mean", "median", "min", "max", "quantile")
      if (object$statlines[[j]]$stat %in% possible_fixed_stats) {
        p <- p + .statline(data_long,
          line = object$statlines[[j]],
          object$statlines[[j]]$variable,  object$mvar, object$pvar,
          fun = object$statlines[[j]]$stat,
          reference_phase = object$statlines[[j]]$phase
        )
      }

      if (object$statlines[[j]]$stat == "trend") {
        p <- p + .statline_trend(
          data_long, object$statlines[[j]],
          object$statlines[[j]]$variable, object$mvar, object$pvar
        )
      }

      if (object$statlines[[j]]$stat %in% "trendA") {
        p <- p + .statline_trend_one(
          data_long, object$statlines[[j]],
          object$statlines[[j]]$variable, object$mvar, object$pvar
        )
      }

      if (object$statlines[[j]]$stat == "movingMean") {
        p <- p + .statline_moving_average(
          data_long, object$statlines[[j]],
          object$statlines[[j]]$variable, object$mvar, object$pvar, "mean"
        )
      }

      if (object$statlines[[j]]$stat == "movingMedian") {
        p <- p + .statline_moving_average(
          data_long, object$statlines[[j]],
          object$statlines[[j]]$variable, object$mvar, object$pvar, "median"
        )
      }

      if (object$statlines[[j]]$stat %in% c("loreg", "lowess")) {
        p <- p + .statline_loreg(
          data_long, object$statlines[[j]],
          object$statlines[[j]]$variable, object$mvar, object$pvar,
          fun = "lowess"
        )
      }

      if (object$statlines[[j]]$stat == "loess") {
        p <- p + .statline_loreg(
          data_long, object$statlines[[j]],
          object$statlines[[j]]$variable, object$mvar, object$pvar,
          fun = "loess"
        )
      }

    }
  }


  # plot background --------------------------------------------------------

  p <- p + theme(plot.background = theme$plot.background)



  # add marks -----

  if (!is.null(object$marks)) {

    cases <- unique(data_long$case)

    for(i in seq_along(object$marks)) {

      dat <- data_long

      # filter cases
      if (!identical(object$marks[[i]]$case, "all"))
        dat <- dat[dat$case %in% cases[object$marks[[i]]$case], ]

      # filter mt
      if (is.character(object$marks[[i]]$positions)) {
        filter <- eval(
          str2expression(object$marks[[i]]$positions), envir = dat
        )
      } else {
        filter <- dat[[mvar]] %in% object$marks[[i]]$position
      }

      dat <- dat[filter, ]

      if (object$marks[[i]]$variable == ".dvar")
        object$marks[[i]]$variable <- dvar

      names(dat)[which(names(dat) == object$marks[[i]]$variable)] <- "dvar"
      names(dat)[which(names(dat) == mvar)] <- "mvar"

      p <- p + geom_point(
        data = dat,
        mapping = aes(x = mvar, y = dvar),
        color = object$marks[[i]]$color,
        size = object$marks[[i]]$size,
        shape = object$marks[[i]]$shape
      )
    }

  }

  # add text annotate -----

  if (length(object$texts) > 0) {

    for(i in seq_along(object$texts)) {
      dat <- data.frame(
        x = object$texts[[i]]$x,
        y = object$texts[[i]]$y,
        label = object$texts[[i]]$labels,
        case = unique(data_long$case)[object$texts[[i]]$case]
      )

      p <- p + geom_text(
        data = dat,
        mapping = aes(x = x, y = y, label = label),
        colour = object$texts[[i]]$colour,
        size = .size(object$texts[[i]]$size, base_size),
        angle = object$texts[[i]]$angle

      )
    }
  }


  # add arrows ---------

  if (length(object$arrows) > 0) {
    for(i in seq_along(object$arrows)) {
      dat <- data.frame(
        x0 = object$arrows[[i]]$x0,
        y0 = object$arrows[[i]]$y0,
        x1 = object$arrows[[i]]$x1,
        y1 = object$arrows[[i]]$y1,
        case = unique(data_long$case)[object$arrows[[i]]$case]
      )

      arrow_par <- arrow(
        object$arrows[[i]]$angle,
        object$arrows[[i]]$length,
        object$arrows[[i]]$ends,
        object$arrows[[i]]$type
      )

      p <- p + geom_segment(
        data = dat,
        mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
        colour = object$arrows[[i]]$colour,
        arrow = arrow_par
      )
    }
  }



  # add legend ------

  .color <- unlist(lapply(object$datalines, function(x) x$col))
  .color <- setNames(.color, .color)
  labels <- unlist(lapply(object$datalines, function(x) x$variable))

  if (!is.null(object$statlines)) {
    labels_statlines <-
      paste(
        unlist(lapply(object$statlines, function(x) x$stat)),
        unlist(lapply(object$statlines, function(x) x$variable))
      )

    labels <- c(labels, labels_statlines)

    .color <- c(
      .color,
      setNames(
        unlist(lapply(object$statlines, function(x) x$line$colour)),
        labels_statlines
      )
    )
  }
  p <- p +
    scale_colour_manual(
      values = .color,
      labels = labels
    )

  if (!is.null(object$legend)) {
    p <- p +
      theme(legend.position = theme$legend.position,
            legend.background = theme$legend.background,
            legend.text = theme$legend.text,
            legend.title = theme$legend.title,
            legend.margin = theme$legend.margin)
    p <- p + guides(fill = guide_legend(title = "Phases"))
    p <- p + guides(colour = guide_legend(title = "Lines"))
  } else p <- p + theme(legend.position = "None")

  # out -----------

  p
}
