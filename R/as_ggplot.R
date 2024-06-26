#' Creates a ggplot2 object from an [scplot()] object
#'
#' @param scplot An `scplot` object
#'
#' @details `as_ggplot()` is used when you want to return a ggplot2 object for
#'   further use with external ggplot functions.
#' @return A `ggplot2` plot object.
#' @export
as_ggplot <- function(scplot) {

  # set global variables --------

  scdf <- scplot$scdf
  theme <- scplot$theme
  dvar <- scplot$dvar
  pvar <- scplot$pvar
  mvar <- scplot$mvar

  base_size <- theme$text$size

  n_cases <- length(scdf)

  # rename casesnames --------

  id <- which(duplicated(scplot$casenames$labels))
  if (length(id) > 0) scplot$casenames$labels[id] <- paste0(".case ", id)
  names(scdf) <- scplot$casenames$labels

  # rename phasenames ----------

  if (!identical(scplot$phasenames$labels, ".default")) {
    for(i in seq_along(scdf)) {
      scdf[[i]][[pvar]] <- factor(scdf[[i]][[pvar]])
      levels(scdf[[i]][[pvar]]) <- scplot$phasenames$labels
    }
  }

  for(i in seq_along(scdf)) {
    scdf[[i]][[pvar]] <- rename_phase_duplicates(
      rle(as.character(scdf[[i]][[pvar]]))$values,
      rle(as.character(scdf[[i]][[pvar]]))$lengths
    )
  }


  # convert to long format --------

  data_long <- as.data.frame(scdf)
  data_long$case <- factor(data_long$case, levels = scplot$casenames$labels)
  data_long[[pvar]] <- factor(data_long[[pvar]])

  attr(data_long, "pvar") <- pvar
  attr(data_long, "mvar") <- mvar


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

  design <- lapply(scdf, .design, pvar = pvar, mvar = mvar)

  # set dataline for first dvar ---------------------

  scplot$datalines[[1]]$variable <- scplot$dvar[1]

  # set x/y label --------

  if (is.null(scplot$xlabel)) {
    scplot$xlabel <- mvar
    if (scplot$xlabel == "mt") scplot$xlabel <- "Measurement time"
    scplot$xlabel <- .upperfirst(scplot$xlabel)
  }
  if (is.null(scplot$ylabel)) {
    if (length(scplot$dvar) == 1) {
      scplot$ylabel <- scplot$dvar
      scplot$ylabel <- .upperfirst(scplot$ylabel)
    } else {
      scplot$ylabel <- "Values"
    }
  }

  # compute global xlim and ylim ---------

  ylim <- scplot$yaxis$lim
  xlim <- scplot$xaxis$lim

  if (is.null(ylim)) {
    .dv <- unlist(lapply(scdf, function(x) x[, scplot$dvar]))
    ylim <- c(min(.dv, na.rm = TRUE), max(.dv, na.rm = TRUE))
  }

  if (is.null(xlim)) {
    .mt     <- unlist(lapply(scdf, function(x) x[, mvar]))
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

  if (!is.null(scplot$yaxis$inc)) {
    p <- p + scale_y_continuous(
      limits = c(ylim[1], ylim[2]),
      breaks = seq(ylim[1], ylim[2], scplot$yaxis$inc),
      expand = theme$axis.expand.y
    )
  } else {
    p <- p + scale_y_continuous(limits = c(ylim[1], ylim[2]),
                                expand = theme$axis.expand.y)
  }

  p <- p + theme(axis.text.x= theme$axis.text.x)

  # set xaxis ticks and text  --------

  if (!is.null(scplot$xaxis$inc_from)) {
    x <- seq(scplot$xaxis$inc_from, xlim[2], scplot$xaxis$inc)
    x <- x[x >= xlim[1]]
    x <- c(1, x)
  } else {
    x <- seq(xlim[1], xlim[2], scplot$xaxis$inc)
  }

  p <- p + scale_x_continuous(
    breaks = x, #seq(xlim[1], xlim[2], scplot$xaxis$inc),
    limits = c(xlim[1], xlim[2]),
    expand = theme$axis.expand.x
  )

  p <- p + theme(axis.text.y = theme$axis.text.y)
  #p <- p + xlim(xlim[1], xlim[2])
  #p <- p + ylim(ylim[1], ylim[2])
  #p <- p + expand_limits(x = xlim, y=ylim)

  # create facets --------------------

  #p <- p + facet_grid(as.factor(case) ~ ., scales = "free", strip.position = theme$casenames.position)

  p <- p + facet_wrap(
    factor(case)~.,
    ncol = 1,
    scales = "free",
    strip.position =
      if (identical(theme$casenames.position, "strip-right")) "right"
      else if (identical(theme$casenames.position, "strip-top")) "top"
      else "right"
  )
  p <- p + theme(panel.spacing.y = theme$panel.spacing.y)
  p <- p + theme(strip.text.y = theme$casenames)
  p <- p + theme(strip.background = theme$casenames.strip)

  if (!theme$casenames.position[1] %in% c("strip-right","strip-top")) {
    p <- p + theme(strip.text.y = element_blank())
    p <- p + theme(strip.text.x = element_blank())

  }


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
      inherit.aes = FALSE,
      show.legend = if (isFALSE(scplot$legend$phases)) FALSE else NA
    )
  }

  p <- p + theme(panel.background = theme$panel.background)

  # add ridges ---------------------------

  if (!is.null(scplot$ridges)) {
    for(i in seq_along(scplot$ridges)) {
      if (scplot$ridges[[i]]$variable == ".dvar")
        scplot$ridges[[i]]$variable <- dvar
      p <- p + geom_ribbon(
        aes(ymax = !!as.name(scplot$ridges[[i]]$variable),
            ymin = ylim[1],
            group = !!as.name(pvar)),
        fill = scplot$ridges[[i]]$colour
      )
    }
  }

  # add dataline and points ---------------------------

  for (i in 1:length(scplot$datalines)) {
    if(scplot$datalines[[i]]$type == "continuous") {
      p <- p + geom_line(
        aes(
          y = !!as.name(scplot$datalines[[i]]$variable),
          group = !!as.name(pvar),
          colour = !!theme$dataline[[i]]$colour
        ),
        linewidth = theme$dataline[[i]]$linewidth,
        linetype = theme$dataline[[i]]$linetype
        #na.rm = TRUE
      )
    }

    if(scplot$datalines[[i]]$type == "discrete") {
      p <- p + geom_step(
        aes(
          y = !!as.name(scplot$datalines[[i]]$variable),
          group = !!as.name(pvar),
          colour = !!theme$dataline[[i]]$colour
        ),
        linewidth = theme$dataline[[i]]$linewidth,
        linetype = theme$dataline[[i]]$linetype
      )
    }

    if(scplot$datalines[[i]]$type == "bar") {
      #suppressWarnings(p <- p + scale_x_discrete())
      p <- p + geom_bar(
        aes(
          #x = factor(!!sym(mvar)),
          y = !!as.name(scplot$datalines[[i]]$variable),
          colour = !!theme$dataline[[i]]$colour
        ),
        #fill = theme$dataline[[i]]$colour,
        stat = "identity",
        #position = position_nudge(x = 0.5),
        width = theme$dataline[[i]]$linewidth,
        linetype = theme$dataline[[i]]$linetype
      )
    }

    # add datapoints

    if (!identical(theme$datapoint[[i]], "none")) {
      p <- p + geom_point(
        aes(y = !!sym(scplot$datalines[[i]]$variable)),
        colour = theme$datapoint[[i]]$colour,
        size = theme$datapoint[[i]]$size,
        shape = theme$datapoint[[i]]$shape
        #na.rm = TRUE
      )
    }

  }

  # add value labels ---------------------------

  if (!is.null(scplot$labels)) {

    for(i in seq_along(scplot$labels)){

      label <- scplot$labels[[i]]
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

  if (!theme$casenames.position[1] %in% c("strip-right","strip-top", "none")) {

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

    if(length(theme$casenames.position) == 2) {
      x <- theme$casenames.position[1]
      y <- theme$casenames.position[2]
    }

    data_casenames <- data.frame(
      x = rep(x, n_cases),
      y = rep(y, n_cases),
      case = scplot$casenames$labels
    )

    if (is.null(theme$casenames$size)) theme$casenames$size <- 1
    theme$casenames <- merge_element(theme$casenames, theme$text)

    if (!is.null(theme$casenames.background$fill)){
      p <- p + geom_label(
        data = data_casenames,
        mapping = aes(x = x, y = y, label =  case),
        colour =  theme$casenames$colour,
        fill = theme$casenames.background$fill,
        label.size = theme$casenames.background$size,
        size = .size(theme$casenames$size, base_size),
        hjust = theme$casenames$hjust,
        vjust = theme$casenames$vjust,
        lineheight = theme$casenames$lineheight,
        family = theme$casenames$family,
        fontface = theme$casenames$face,
        angle = theme$casenames$angle
      )
    } else {
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
    linetype = theme$separators$linetype,
    color = theme$separators$colour,
    linewidth = theme$separators$linewidth
  )

  p <- p + coord_cartesian(clip = "off")

  # add phasenames ---------

  if (!identical(theme$phasenames.position.x, "none")) {

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

  }

  # add axis.line -----------

  p <- p + theme(axis.line.x = theme$axis.line.x)
  p <- p + theme(axis.line.y = theme$axis.line.y)
  p <- p + theme(axis.ticks.length = theme$axis.ticks.length)
  p <- p + theme(axis.ticks = theme$axis.ticks)

  # add grid ------------

  p <- p + theme(panel.grid = theme$grid)

  # add title ------------------------

  if (!is.null(scplot$title)) {
    p <- p + ggtitle(scplot$title) +
      theme(plot.title = theme$plot.title)
  }

  # add caption -------------

  if (!is.null(scplot$caption)) {
    p <- p + labs(caption = scplot$caption) +
      theme(plot.caption = theme$plot.caption, plot.caption.position = "plot")

  }

  # add axis label ------

  if (!is.null(scplot$ylabel)) p <- p + ylab(scplot$ylabel)
  if (!is.null(scplot$xlabel)) p <- p + xlab(scplot$xlabel)

  p <- p + theme(axis.title.y = theme$axis.title.y)
  p <- p + theme(axis.title.x = theme$axis.title.x)

  # add statlines ------------------------------------------------------------

  if (!is.null(scplot$statlines)) {

    for(j in 1:length(scplot$statlines)) {
      scplot$statlines[[j]]$line <- theme$statline[[j]]
      if (scplot$statlines[[j]]$variable == ".dvar") {
        scplot$statlines[[j]]$variable <- scplot$dvar[1]
      }

      # by constant
      .constant_stats <- c(
        "mean", "median", "min", "max", "quantile", "sd", "mad"
      )
      if (scplot$statlines[[j]]$stat %in% .constant_stats) {
        p <- p + .statline_constant(
          data_long,
          line = scplot$statlines[[j]],
          fun = scplot$statlines[[j]]$stat,
          reference_phase = scplot$statlines[[j]]$phase
        )
      }

      # trend
      if (scplot$statlines[[j]]$stat == "trend") {
        p <- p + .statline_trend_by_phase(
          data_long,
          line = scplot$statlines[[j]]
        )
      }

      # by continuous
      .continuous_stats <- c(
        "moving mean", "movingMean", "moving median", "movingMedian",
        "trendA", "trendA (Theil-Sen)", "trendA_bisplit", "trendA_trisplit",
        "trendA bisplit", "trendA trisplit", "trendA theil-sen",
        "loreg", "lowess", "loess"
      )
      if (scplot$statlines[[j]]$stat %in% .continuous_stats) {
        p <- p + .statline_continuous(
          data_long,
          line = scplot$statlines[[j]],
          fun = scplot$statlines[[j]]$stat
        )
      }

    }
  }


  # plot background --------------------------------------------------------

  p <- p + theme(plot.background = theme$plot.background)



  # add marks -----

  if (!is.null(scplot$marks)) {

    cases <- unique(data_long$case)

    for(i in seq_along(scplot$marks)) {

      dat <- data_long

      # filter cases
      if (!identical(scplot$marks[[i]]$case, "all"))
        dat <- dat[dat$case %in% cases[scplot$marks[[i]]$case], ]

      # filter mt
      if (is.character(scplot$marks[[i]]$positions)) {
        filter <- eval(
          str2expression(scplot$marks[[i]]$positions), envir = dat
        )
      } else {
        filter <- dat[[mvar]] %in% scplot$marks[[i]]$position
      }

      dat <- dat[filter, ]

      if (scplot$marks[[i]]$variable == ".dvar")
        scplot$marks[[i]]$variable <- dvar

      names(dat)[which(names(dat) == scplot$marks[[i]]$variable)] <- "dvar"
      names(dat)[which(names(dat) == mvar)] <- "mvar"

      p <- p + geom_point(
        data = dat,
        mapping = aes(x = mvar, y = dvar),
        color = scplot$marks[[i]]$color,
        size = scplot$marks[[i]]$size,
        shape = scplot$marks[[i]]$shape
        #na.rm = TRUE
      )
    }

  }

  # add text annotate -----

  if (length(scplot$texts) > 0) {

    for(i in seq_along(scplot$texts)) {
      dat <- data.frame(
        x = scplot$texts[[i]]$x,
        y = scplot$texts[[i]]$y,
        label = scplot$texts[[i]]$labels,
        case = unique(data_long$case)[scplot$texts[[i]]$case]
      )

      p <- p + geom_text(
        data = dat,
        mapping = aes(x = x, y = y, label = label),
        colour = scplot$texts[[i]]$colour,
        size = .size(scplot$texts[[i]]$size, base_size),
        angle = scplot$texts[[i]]$angle,
        hjust = scplot$texts[[i]]$hjust,
        vjust = scplot$texts[[i]]$vjust,
        fontface = scplot$texts[[i]]$face
      )
    }
  }


  # add lines ---------

  if (length(scplot$lines) > 0) {
    for(i in seq_along(scplot$lines)) {

      if (!is.null(scplot$lines[[i]]$hline)) {
        data_hline <- data.frame(
          case = unique(data_long$case)[scplot$lines[[i]]$case],
          y = scplot$lines[[i]]$hline
        )
        p <- p + geom_hline(
          data = data_hline,
          aes(yintercept = y),
          linetype = scplot$lines[[i]]$linetype,
          color = scplot$lines[[i]]$colour,
          linewidth = scplot$lines[[i]]$linewidth
        )
        next
      }

      if (!is.null(scplot$lines[[i]]$vline)) {
        data_vline <- data.frame(
          case = unique(data_long$case)[scplot$lines[[i]]$case],
          x = scplot$lines[[i]]$vline
        )
        p <- p + geom_vline(
          data = data_vline,
          aes(xintercept = x),
          linetype = scplot$lines[[i]]$linetype,
          color = scplot$lines[[i]]$colour,
          linewidth = scplot$lines[[i]]$linewidth
        )
        next
      }

      dat <- data.frame(
        x0 = scplot$lines[[i]]$x0,
        y0 = scplot$lines[[i]]$y0,
        x1 = scplot$lines[[i]]$x1,
        y1 = scplot$lines[[i]]$y1,
        case = unique(data_long$case)[scplot$lines[[i]]$case]
      )

      if (scplot$lines[[i]]$arrow) {
        arrow_par <- arrow(
          scplot$lines[[i]]$angle,
          scplot$lines[[i]]$length,
          scplot$lines[[i]]$ends,
          scplot$lines[[i]]$type
        )
      } else {
        arrow_par <- NULL
      }

      p <- p + geom_segment(
        data = dat,
        mapping = aes(x = x0, y = y0, xend = x1, yend = y1),
        colour = scplot$lines[[i]]$colour,
        size = scplot$lines[[i]]$size,
        arrow = arrow_par
      )
    }
  }



  # add legend ------

  .color <- unlist(
    lapply(theme$dataline[1:length(scplot$datalines)], function(x) x$colour)
  )
  .color <- setNames(.color, .color)
  labels <- unlist(lapply(scplot$datalines, function(x) x$variable))

  if (!is.null(scplot$statlines)) {
    labels_statlines <-
      paste(
        unlist(lapply(scplot$statlines, function(x) x$stat)),
        unlist(lapply(scplot$statlines, function(x) x$variable))
      )

    labels <- c(labels, labels_statlines)

    .color <- c(
      .color,
      setNames(
        unlist(
          lapply(
            theme$statline[1:length(scplot$statlines)],
            function(x) x$colour
          )
        ),
        labels_statlines
      )
    )
  }

  if (!is.null(scplot$legend$labels)) {
    .ids <- which(!is.na(scplot$legend$labels))
    labels[.ids] <- scplot$legend$labels[.ids]
  }

  p <- p +
    scale_colour_manual(
      values = .color,
      labels = labels
    )

  if (!is.null(scplot$legend)) {
    p <- p +
      theme(legend.position = theme$legend.position,
            legend.background = theme$legend.background,
            legend.text = theme$legend.text,
            legend.title = theme$legend.title,
            legend.margin = theme$legend.margin)
    p <- p + guides(
      fill = guide_legend(title = scplot$legend$section_label[2])
    )
    p <- p + guides(
      colour = guide_legend(title = scplot$legend$section_label[1])
    )
  } else p <- p + theme(legend.position = "None")

  # out -----------

  p
}

rename_phase_duplicates <- function(phase_labels, phase_lengths) {
  while(TRUE) {
    id <- duplicated(phase_labels)
    if (all(!id)) break
    phase_labels[id] <- paste0(phase_labels[id], " ")
  }
  mapply(
    function(x,y) rep(x, y),
    x = phase_labels,
    y = phase_lengths,
    SIMPLIFY = FALSE
  ) |> unlist() |> unname() |> as.factor()
}

